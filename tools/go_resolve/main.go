package main

import (
	"encoding/json"
	"fmt"
	"go/ast"
	"go/build"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"os"
)
type type_ref struct {
	Kind       string    `json:"kind"`
	Basic      string    `json:"basic,omitempty"`
	Ref        string    `json:"ref,omitempty"`
	Elem       *type_ref  `json:"elem,omitempty"`
	Len        *int64    `json:"len,omitempty"`
	Key        *type_ref  `json:"key,omitempty"`
	Value      *type_ref  `json:"value,omitempty"`
	Fields     []field   `json:"fields,omitempty"`
	Methods    []method  `json:"methods,omitempty"`
	Signature  *func_type `json:"signature,omitempty"`
	Dir        string    `json:"dir,omitempty"`
	Name       string    `json:"name,omitempty"`
	Constraint string    `json:"constraint,omitempty"`
}

type field struct {
	Name     string  `json:"name"`
	Type     type_ref `json:"type"`
	Exported bool    `json:"exported"`
	Embedded bool    `json:"embedded"`
	Tag      string  `json:"tag,omitempty"`
}

type method struct {
	Name string   `json:"name"`
	Type func_type `json:"type"`
}

type type_param struct {
	Name       string `json:"name"`
	Constraint string `json:"constraint,omitempty"`
}

type func_type struct {
	TypeParams []type_param `json:"type_params,omitempty"`
	Variadic   bool        `json:"variadic"`
	Params     []type_ref   `json:"params"`
	Results    []type_ref   `json:"results"`
}

type package_output struct {
	Package   string              `json:"package"`
	Types     map[string]type_ref  `json:"types"`
	Functions map[string]func_type `json:"functions"`
}

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintln(os.Stderr, "usage: go_resolve <package-import-path>")
		os.Exit(1)
	}

	pkg_path := os.Args[1]

	result, err := resolve_pkg(pkg_path)
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}

	json_encoder := json.NewEncoder(os.Stdout)
	if err := json_encoder.Encode(result); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}

func resolve_pkg(pkg_path string) (*package_output, error) {
	build_pkg, err := build.Default.Import(pkg_path, "", 0)
	if err != nil {
		return nil, fmt.Errorf("could not load package %q: %w", pkg_path, err)
	}

	file_set := token.NewFileSet()

	var files []*ast.File
	for _, name := range build_pkg.GoFiles {
		path := build_pkg.Dir + string(os.PathSeparator) + name

		file, err := parser.ParseFile(file_set, path, nil, 0)
		if err != nil {
			return nil, fmt.Errorf("could not parse %q: %w", path, err)
		}

		files = append(files, file)
	}

	config := types.Config{Importer: importer.Default()}

	types_pkg, err := config.Check(pkg_path, file_set, files, nil)
	if err != nil {
		return nil, fmt.Errorf("could not type-check package %q: %w", pkg_path, err)
	}

	coll := &collector{types: make(map[string]type_ref)}
	functions := make(map[string]func_type)

	scope := types_pkg.Scope()
	for _, name := range scope.Names() {
		if !ast.IsExported(name) {
			continue
		}

		instanc := scope.Lookup(name)

		switch obj := instanc.(type) {
		case *types.Func:
			sig, ok := obj.Type().(*types.Signature)
			if !ok {
				continue
			}

			if sig.Recv() != nil {
				continue
			}

			functions[name] = coll.describeFunc(sig)

		case *types.TypeName:
			if obj.IsAlias() {
				continue
			}

			named, ok := obj.Type().(*types.Named)
			if !ok {
				continue
			}

			coll.describe(named)
		}
	}

	return &package_output{Package: pkg_path, Types: coll.types, Functions: functions}, nil
}

type collector struct {
	types map[string]type_ref
}

func quialified_name(named *types.Named) string {
	instance := named.Obj()
	if pkg := instance.Pkg(); pkg != nil {
		return pkg.Path() + "." + instance.Name()
	}

	return instance.Name()
}

func (c *collector) describe(type_ types.Type) type_ref {
	type_ = types.Unalias(type_)

	switch type_ := type_.(type) {
	case *types.Basic:
		return type_ref{Kind: "basic", Basic: type_.Name()}

	case *types.Named:
		name := quialified_name(type_)
		if _, ok := c.types[name]; !ok {
			c.types[name] = type_ref{Kind: "unsupported"}
			c.types[name] = c.describe(type_.Underlying())
		}

		return type_ref{Kind: "named", Ref: name}

	case *types.Pointer:
		elem := c.describe(type_.Elem())
		return type_ref{Kind: "pointer", Elem: &elem}

	case *types.Slice:
		elem := c.describe(type_.Elem())
		return type_ref{Kind: "slice", Elem: &elem}

	case *types.Array:
		elem := c.describe(type_.Elem())
		length := type_.Len()
		return type_ref{Kind: "array", Elem: &elem, Len: &length}

	case *types.Map:
		key := c.describe(type_.Key())
		value := c.describe(type_.Elem())
		return type_ref{Kind: "map", Key: &key, Value: &value}

	case *types.Chan:
		elem := c.describe(type_.Elem())

		dir := "both"
		switch type_.Dir() {
		case types.SendOnly:
			dir = "send"
		case types.RecvOnly:
			dir = "recv"
		}

		return type_ref{Kind: "chan", Elem: &elem, Dir: dir}

	case *types.Struct:
		fields := make([]field, 0, type_.NumFields())
		for i := 0; i < type_.NumFields(); i++ {
			fild := type_.Field(i)
			fields = append(fields, field{
				Name:     fild.Name(),
				Type:     c.describe(fild.Type()),
				Exported: fild.Exported(),
				Embedded: fild.Embedded(),
				Tag:      type_.Tag(i),
			})
		}
		return type_ref{Kind: "struct", Fields: fields}

	case *types.Interface:
		type_.Complete()
		if type_.NumMethods() == 0 {
			return type_ref{Kind: "any"}
		}
		methods := make([]method, 0, type_.NumMethods())
		for i := 0; i < type_.NumMethods(); i++ {
			m := type_.Method(i)

			sig, ok := m.Type().(*types.Signature)
			if !ok {
				continue
			}

			methods = append(methods, method{Name: m.Name(), Type: c.describeFunc(sig)})
		}

		return type_ref{Kind: "interface", Methods: methods}

	case *types.Signature:
		sig := c.describeFunc(type_)
		return type_ref{Kind: "func", Signature: &sig}

	case *types.TypeParam:
		constraint := ""
		if iface := type_.Constraint(); iface != nil {
			constraint = iface.String()
		}

		return type_ref{Kind: "type_param", Name: type_.Obj().Name(), Constraint: constraint}

	default:
		return type_ref{Kind: "unsupported"}
	}
}

func (c *collector) describeFunc(sig *types.Signature) func_type {
	var type_params []type_param
	if tp := sig.TypeParams(); tp != nil {
		for i := 0; i < tp.Len(); i++ {
			p := tp.At(i)

			constraint := ""
			if p.Constraint() != nil {
				constraint = p.Constraint().String()
			}

			type_params = append(type_params, type_param{Name: p.Obj().Name(), Constraint: constraint})
		}
	}

	params_len := sig.Params().Len()
	params := make([]type_ref, 0, params_len)
	for i := 0; i < params_len; i++ {
		params = append(params, c.describe(sig.Params().At(i).Type()))
	}

	results_len := sig.Results().Len()
	results := make([]type_ref, 0, results_len)
	for i := 0; i < results_len; i++ {
		results = append(results, c.describe(sig.Results().At(i).Type()))
	}

	return func_type{
		TypeParams: type_params,
		Variadic:   sig.Variadic(),
		Params:     params,
		Results:    results,
	}
}
