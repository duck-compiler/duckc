package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"go/importer"
	"go/token"
	"go/types"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

type ParamJSON struct {
	Name     string `json:"name,omitempty"`
	Type     string `json:"type"`
	Variadic bool   `json:"variadic,omitempty"`
}

type FuncJSON struct {
	Name    string      `json:"name"`
	Params  []ParamJSON `json:"params"`
	Results []ParamJSON `json:"results"`
}

type FieldJSON struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type TypeJSON struct {
	Name    string      `json:"name"`
	Kind    string      `json:"kind"` // "struct", "interface", "alias", "other"
	Fields  []FieldJSON `json:"fields,omitempty"`
	Methods []FuncJSON  `json:"methods,omitempty"`
}

type VarJSON struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type PackageJSON struct {
	ImportPath string     `json:"import_path"`
	Functions  []FuncJSON `json:"functions"`
	Types      []TypeJSON `json:"types"`
	Vars       []VarJSON  `json:"vars"`
	Consts     []VarJSON  `json:"consts"`
}

var qualifier = func(p *types.Package) string { return p.Name() }

func typeStr(t types.Type) string {
	return types.TypeString(t, qualifier)
}

func extractSig(name string, sig *types.Signature) FuncJSON {
	f := FuncJSON{Name: name}
	p := sig.Params()
	variadic := sig.Variadic()
	for i := 0; i < p.Len(); i++ {
		v := p.At(i)
		ty := v.Type()
		isLast := i == p.Len()-1
		isVariadic := variadic && isLast
		if isVariadic {
			if sl, ok := ty.(*types.Slice); ok {
				ty = sl.Elem()
			}
		}
		f.Params = append(f.Params, ParamJSON{
			Name:     v.Name(),
			Type:     typeStr(ty),
			Variadic: isVariadic,
		})
	}
	r := sig.Results()
	for i := 0; i < r.Len(); i++ {
		v := r.At(i)
		f.Results = append(f.Results, ParamJSON{
			Name: v.Name(),
			Type: typeStr(v.Type()),
		})
	}
	return f
}

// goExePath returns the path to the `go` binary, preferring GOROOT if set.
func goExePath() string {
	goroot := os.Getenv("GOROOT")
	if goroot == "" {
		return "go"
	}
	ext := ""
	if runtime.GOOS == "windows" {
		ext = ".exe"
	}
	candidate := filepath.Join(goroot, "bin", "go"+ext)
	if _, err := os.Stat(candidate); err == nil {
		return candidate
	}
	return "go"
}

// buildExportMap runs `go list -json -deps -export <importPath>` in workDir and
// returns a map from import path to compiled export-file path for every package
// in the dependency graph. This allows the gc importer to resolve the package
// and all its transitive dependencies without needing them in GOROOT.
func buildExportMap(importPath, workDir string) (map[string]string, error) {
	goBin := goExePath()
	cmd := exec.Command(goBin, "list", "-json", "-deps", "-export", "-buildvcs=false", importPath)
	cmd.Dir = workDir
	cmd.Env = os.Environ()

	var stderr bytes.Buffer
	cmd.Stderr = &stderr

	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("go list failed: %v\n%s", err, stderr.String())
	}

	type listPkg struct {
		ImportPath string `json:"ImportPath"`
		Export     string `json:"Export"`
	}

	exportMap := make(map[string]string)
	dec := json.NewDecoder(bytes.NewReader(out))
	for dec.More() {
		var p listPkg
		if err := dec.Decode(&p); err != nil {
			break
		}
		if p.Export != "" {
			exportMap[p.ImportPath] = p.Export
		}
	}
	return exportMap, nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: go-type-info <import-path> [<working-dir>]")
		os.Exit(1)
	}
	importPath := os.Args[1]
	workDir := ""
	if len(os.Args) >= 3 {
		workDir = os.Args[2]
	}

	fset := token.NewFileSet()

	var imp types.Importer
	if workDir != "" {
		exportMap, err := buildExportMap(importPath, workDir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "cannot build export map for %s: %v\n", importPath, err)
			os.Exit(1)
		}
		imp = importer.ForCompiler(fset, "gc", func(path string) (io.ReadCloser, error) {
			if f, ok := exportMap[path]; ok {
				return os.Open(f)
			}
			return nil, fmt.Errorf("no export file for %s", path)
		})
	} else {
		imp = importer.ForCompiler(fset, "gc", nil)
	}

	pkg, err := imp.Import(importPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cannot import %s: %v\n", importPath, err)
		os.Exit(1)
	}

	out := PackageJSON{
		ImportPath: importPath,
		Functions:  []FuncJSON{},
		Types:      []TypeJSON{},
		Vars:       []VarJSON{},
		Consts:     []VarJSON{},
	}
	scope := pkg.Scope()

	for _, name := range scope.Names() {
		obj := scope.Lookup(name)
		if !obj.Exported() {
			continue
		}
		switch o := obj.(type) {
		case *types.Func:
			sig := o.Type().(*types.Signature)
			out.Functions = append(out.Functions, extractSig(name, sig))

		case *types.TypeName:
			ti := TypeJSON{Name: name}
			under := o.Type().Underlying()
			switch u := under.(type) {
			case *types.Struct:
				ti.Kind = "struct"
				for i := 0; i < u.NumFields(); i++ {
					f := u.Field(i)
					if f.Exported() {
						ti.Fields = append(ti.Fields, FieldJSON{
							Name: f.Name(),
							Type: typeStr(f.Type()),
						})
					}
				}
				// Collect value + pointer receiver methods without duplicates.
				seen := map[string]bool{}
				if named, ok := o.Type().(*types.Named); ok {
					for i := 0; i < named.NumMethods(); i++ {
						m := named.Method(i)
						if m.Exported() && !seen[m.Name()] {
							seen[m.Name()] = true
							ti.Methods = append(ti.Methods, extractSig(m.Name(), m.Type().(*types.Signature)))
						}
					}
					ms := types.NewMethodSet(types.NewPointer(named))
					for i := 0; i < ms.Len(); i++ {
						m, ok := ms.At(i).Obj().(*types.Func)
						if ok && m.Exported() && !seen[m.Name()] {
							seen[m.Name()] = true
							ti.Methods = append(ti.Methods, extractSig(m.Name(), m.Type().(*types.Signature)))
						}
					}
				}
			case *types.Interface:
				ti.Kind = "interface"
				for i := 0; i < u.NumMethods(); i++ {
					m := u.Method(i)
					if m.Exported() {
						ti.Methods = append(ti.Methods, extractSig(m.Name(), m.Type().(*types.Signature)))
					}
				}
			default:
				ti.Kind = "alias"
			}
			out.Types = append(out.Types, ti)

		case *types.Var:
			out.Vars = append(out.Vars, VarJSON{Name: name, Type: typeStr(o.Type())})

		case *types.Const:
			out.Consts = append(out.Consts, VarJSON{Name: name, Type: typeStr(o.Type())})
		}
	}

	enc := json.NewEncoder(os.Stdout)
	if err := enc.Encode(out); err != nil {
		fmt.Fprintf(os.Stderr, "encode error: %v\n", err)
		os.Exit(1)
	}
}
