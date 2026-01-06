package main
import (
 "hash/maphash"

 "sync"
 "runtime"
 "fmt"

 "time"
 "strings"

 "os"
 "net/http"
 "bytes"
 "io/ioutil"

 "unsafe"
 "sync/atomic"
 "os/exec"
 "path/filepath"


)
func  main()  {

func() Tup_ {
var var_2874 Tup_
var_2874 = Tup_{}
_ = var_2874
{
var var_2815 []string
_ = var_2815

        var_2815 = os.Args

var var_2816 []string
_ = var_2816
var_2816 = var_2815
var args []string
_ = args
args = var_2816
var var_2817 func(flag string) string
_ = var_2817
var_2817 = func(flag string) string {
var var_2820 string
_ = var_2820
{
var var_2818 string
_ = var_2818
var_2818 = "--"
var var_2819 string
_ = var_2819
var_2819 = Extend_string_with_trim_prefix(flag)(var_2818)
var_2820 = var_2819
}
return var_2820
var ΔΔΔretΔΔΔ *string
return *ΔΔΔretΔΔΔ
}
var var_2821 func(arg string) bool
_ = var_2821
var_2821 = func(arg string) bool {
var var_2824 bool
_ = var_2824
{
var var_2822 string
_ = var_2822
var_2822 = "--"
var var_2823 bool
_ = var_2823
var_2823 = Extend_string_with_starts_with(arg)(var_2822)
var_2824 = var_2823
}
return var_2824
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}
var var_2825 []string
_ = var_2825
var_2825 = Array_string_Copy((args))
var flags *Struct_std_____col_____ArrayList_____string
_ = flags
flags = std_____col_____ArrayList_____from_array_____string(var_2825).filter(var_2821).map_____string(var_2817)
var var_2826 string
_ = var_2826
var_2826 = "cicd"
var cicd_mode bool
_ = cicd_mode
cicd_mode = flags.contains(var_2826)
var wg sync.WaitGroup
_ = wg
var var_2827 Tup_
var_2827 = Tup_{}
_ = var_2827

        num_cpus := runtime.NumCPU()
        semaphore := make(chan struct{}, num_cpus)

var results *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = results
results = std_____col_____ArrayList_____new_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath()
var var_2828 func(fp *Struct_std_____path_____FilePath) Tup_
_ = var_2828
var_2828 = func(fp *Struct_std_____path_____FilePath) Tup_ {
var var_2867 Tup_
var_2867 = Tup_{}
_ = var_2867
{
var var_2829 func() Tup_
_ = var_2829
var_2829 = func() Tup_ {
var var_2865 Tup_
var_2865 = Tup_{}
_ = var_2865
{
var var_2830 string
_ = var_2830
var_2830 = "."
var var_2831 string
_ = var_2831
var_2831 = "_"
var var_2832 string
_ = var_2832
var_2832 = "/"
var var_2833 string
_ = var_2833
var_2833 = "_"
var binary_name string
_ = binary_name
binary_name = Extend_string_with_replace(Extend_string_with_replace(fp.path_ref)(var_2832, var_2833))(var_2830, var_2831)
var var_2834 string
_ = var_2834
var_2834 = "couldn't run dargo - are you sure it's installed on this system?"
var var_2836 string
_ = var_2836
{
var var_2835 string
_ = var_2835
var_2835 = IDENTITY((binary_name))
var_2836 = var_2835
}
var var_2838 string
_ = var_2838
{
var var_2837 *Struct_std_____path_____FilePath
_ = var_2837
var_2837 = Struct_std_____path_____FilePath_Copy((fp))
var_2838 = var_2837.path_ref
}
var var_2839 string
_ = var_2839
var_2839 = "dargo compile --output-name " + var_2836 + " " + var_2838
var dargo_output interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = dargo_output
dargo_output = std_____cmd_____Cmd_____exec(var_2839).expect(var_2834)
var var_2840 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2840
var_2840 = IDENTITY((dargo_output))
var var_2841 int
_ = var_2841
var_2841 = 0
var var_2842 bool
_ = var_2842
var_2842 = (var_2840.Getexitcode()) == (var_2841)
var var_2843 bool
_ = var_2843
var_2843 = !var_2842
var var_2844 Tup_
var_2844 = Tup_{}
_ = var_2844
if var_2843 {
var var_2846 string
_ = var_2846
{
var var_2845 string
_ = var_2845
var_2845 = IDENTITY((binary_name))
var_2846 = var_2845
}
var var_2847 string
_ = var_2847
var_2847 = "here's something off " + var_2846
std_____error_____panic(var_2847)
}
var var_2849 string
_ = var_2849
{
var var_2848 string
_ = var_2848
var_2848 = IDENTITY((binary_name))
var_2849 = var_2848
}
var var_2851 string
_ = var_2851
{
var var_2850 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2850
var_2850 = IDENTITY((dargo_output))
var_2851 = var_2850.Getstderr()
}
var var_2853 string
_ = var_2853
{
var var_2852 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2852
var_2852 = IDENTITY((dargo_output))
var_2853 = var_2852.Getstdout()
}
var var_2854 string
_ = var_2854
var_2854 = "couldn't run " + var_2849 + " - " + var_2851 + " " + var_2853
var var_2855 string
_ = var_2855
var_2855 = "./.dargo/"
var var_2857 string
_ = var_2857
{
var var_2856 string
_ = var_2856
var_2856 = IDENTITY((binary_name))
var_2857 = var_2856
}
var var_2858 string
_ = var_2858
var_2858 = "./" + var_2857
var binary_output interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = binary_output
binary_output = (*std_____cmd_____Cmd_____new(var_2858).dir(var_2855)).run().expect(var_2854)
var var_2859 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2859
var_2859 = IDENTITY((binary_output))
var var_2860 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2860
var_2860 = IDENTITY((dargo_output))
var var_2861 *Struct_std_____path_____FilePath
_ = var_2861
var_2861 = Struct_std_____path_____FilePath_Copy((fp))
var var_2862 interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2862
var_2862 = &Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath{binary_output: var_2859, dargo_output: var_2860, src_path: var_2861}
var var_2863 **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_2863
var_2863 = results.push(var_2862)
var var_2864 Tup_
var_2864 = Tup_{}
_ = var_2864
var_2864 = Tup_{}
var_2865 = var_2864
}
return var_2865
return Tup_{}
}
var f func() Tup_
_ = f
f = var_2829
var var_2866 Tup_
var_2866 = Tup_{}
_ = var_2866

                wg.Add(1)
                go func() {
                	defer wg.Done()

                 	semaphore <- struct{}{}
                  	defer func() { <-semaphore }()

                 	f();
                }()

var_2867 = var_2866
}
return var_2867
return Tup_{}
}
var var_2868 func(fp *Struct_std_____path_____FilePath) bool
_ = var_2868
var_2868 = func(fp *Struct_std_____path_____FilePath) bool {
var var_2871 bool
_ = var_2871
{
var var_2869 string
_ = var_2869
var_2869 = ".duck"
var var_2870 bool
_ = var_2870
var_2870 = Extend_string_with_ends_with(fp.filename())(var_2869)
var_2871 = var_2870
}
return var_2871
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}
var var_2872 string
_ = var_2872
var_2872 = "./valid_programs/"
var valid_program_files Tup_
valid_program_files = Tup_{}
_ = valid_program_files
valid_program_files = std_____path_____FilePath_____new(var_2872).walk_files().filter(var_2868).for_each(var_2828)
var var_2873 Tup_
var_2873 = Tup_{}
_ = var_2873

       	wg.Wait()

var_2874 = var_2873
}
return var_2874
} ()
}

func  std_____io_____println(str string) Tup_ {
_ = str
var var_2878 Tup_
var_2878 = Tup_{}
_ = var_2878
{
var var_2877 Tup_
var_2877 = Tup_{}
_ = var_2877

        fmt.Println(str);

var_2878 = var_2877
}
return var_2878
return Tup_{}
}















func  std_____error_____panic(m interface{}) any {
_ = m
var var_3348 any
_ = var_3348

        fmt.Println("panic:", m)
        os.Exit(1)

var var_3349 any
_ = var_3349
var_3349 = var_3348
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
func  std_____error_____unreachable(s string) any {
_ = s
var var_3351 string
_ = var_3351
{
var var_3350 string
_ = var_3350
var_3350 = IDENTITY((s))
var_3351 = var_3350
}
var var_3352 string
_ = var_3352
var_3352 = "logic error [unreachable reached]: " + var_3351
var var_3353 Tup_
var_3353 = Tup_{}
_ = var_3353
var_3353 = std_____io_____println(var_3352)
var var_3354 any
_ = var_3354

        os.Exit(1)

var var_3355 any
_ = var_3355
var_3355 = var_3354
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}


func  std_____web_____render(renderer func (env *TemplEnv) string) string {
_ = renderer
var var_3384 Tup_
var_3384 = Tup_{}
_ = var_3384

	    env := TemplEnv{[]string{}, []RenderCall{}}
		res := renderer(&env)

		prelude := `
            <script type="module">
            import { h, render } from "https://esm.sh/preact";
            import { useState } from "https://esm.sh/preact/hooks";
            import htm from "https://esm.sh/htm";

            // Initialize htm with Preact
            const html = htm.bind(h);
        `

        for _, e := range env.ClientComponents {
            prelude += e
            prelude += "\n"
        }

        for _, e := range env.RenderCalls {
            prelude += fmt.Sprintf("document.querySelectorAll(\"[duckx-render=\\\"%s\\\"]\").forEach(e => render(html`%s`, e))", e.Id, e.Jsx)
            prelude += "\n"
        }

        prelude += "\n</script>"

		return res + prelude

var var_3385 string
_ = var_3385
var_3385 = ""
return var_3385
var ΔΔΔretΔΔΔ *string
return *ΔΔΔretΔΔΔ
}

func  std_____web_____Cookie_____from_go_cookie(go_cookie *http.Cookie) *Struct_std_____web_____Cookie {
_ = go_cookie
var name string
_ = name
var value string
_ = value
var quoted bool
_ = quoted
var p string
_ = p
var domain string
_ = domain
var expires any
_ = expires
var raw_expires string
_ = raw_expires
var max_age int
_ = max_age
var secure bool
_ = secure
var http_only bool
_ = http_only
var same_site any
_ = same_site
var partitioned bool
_ = partitioned
var raw string
_ = raw
var unparsed []string
_ = unparsed
var var_3401 Tup_
var_3401 = Tup_{}
_ = var_3401

            name = go_cookie.Name
            value = go_cookie.Value
            quoted = go_cookie.Quoted
            p = go_cookie.Path
            domain = go_cookie.Domain
            if go_cookie.Expires.IsZero() {
            	expires = Tag__none {}
            } else {
            	expires = go_cookie.Expires
            }

            max_age = go_cookie.MaxAge
            secure = go_cookie.Secure
            http_only = go_cookie.HttpOnly
            switch go_cookie.SameSite {
            	case http.SameSiteDefaultMode:
             		same_site = Tag__default {}
             	case http.SameSiteLaxMode:
              		same_site = Tag__lax {}
              	case http.SameSiteStrictMode:
               		same_site = Tag__strict {}
               	case http.SameSiteNoneMode:
                	same_site = Tag__none {}
            }

            partitioned = go_cookie.Partitioned
            raw = go_cookie.Raw
            unparsed = go_cookie.Unparsed

var var_3402 string
_ = var_3402
var_3402 = IDENTITY((name))
var var_3403 string
_ = var_3403
var_3403 = IDENTITY((value))
var var_3404 bool
_ = var_3404
var_3404 = IDENTITY((quoted))
var var_3405 string
_ = var_3405
var_3405 = IDENTITY((p))
var var_3406 string
_ = var_3406
var_3406 = IDENTITY((domain))
var var_3407 any
_ = var_3407
var_3407 = func() any {
                    var p1 any = (expires)

                        switch p1.(type) {
                        case *Struct_std_____time_____Time:
                            tmp := p1.(*Struct_std_____time_____Time)
                            _ = tmp
                            return Struct_std_____time_____Time_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_3408 string
_ = var_3408
var_3408 = IDENTITY((raw_expires))
var var_3409 int
_ = var_3409
var_3409 = IDENTITY((max_age))
var var_3410 bool
_ = var_3410
var_3410 = IDENTITY((secure))
var var_3411 bool
_ = var_3411
var_3411 = IDENTITY((http_only))
var var_3412 any
_ = var_3412
var_3412 = func() any {
                    var p1 any = (same_site)

                        switch p1.(type) {
                        case Tag__default:
                            tmp := p1.(Tag__default)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__lax:
                            tmp := p1.(Tag__lax)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__strict:
                            tmp := p1.(Tag__strict)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_3413 bool
_ = var_3413
var_3413 = IDENTITY((partitioned))
var var_3414 string
_ = var_3414
var_3414 = IDENTITY((raw))
var var_3415 []string
_ = var_3415
var_3415 = Array_string_Copy((unparsed))
var var_3416 *Struct_std_____web_____Cookie
_ = var_3416
var_3416 = &Struct_std_____web_____Cookie{fname: var_3402, fvalue: var_3403, fquoted: var_3404, fpath: var_3405, fdomain: var_3406, fexpires: var_3407, fraw_expires: var_3408, fmax_age: var_3409, fsecure: var_3410, fhttp_only: var_3411, fsame_site: var_3412, fpartitioned: var_3413, fraw: var_3414, funparsed: var_3415}
return var_3416
var ΔΔΔretΔΔΔ **Struct_std_____web_____Cookie
return *ΔΔΔretΔΔΔ
}

func  std_____cmd_____Cmd_____new(program string) *Struct_std_____cmd_____Cmd {
_ = program
var var_3423 string
_ = var_3423
var_3423 = IDENTITY((program))
var var_3424 *Struct_std_____col_____ArrayList_____string
_ = var_3424
var_3424 = std_____col_____ArrayList_____new_____string()
var var_3425 Tag__none
_ = var_3425
var_3425 = Tag__none{}
var var_3426 *Struct_std_____cmd_____Cmd
_ = var_3426
var_3426 = &Struct_std_____cmd_____Cmd{program: var_3423, cmd_args: var_3424, cwd: var_3425}
return var_3426
var ΔΔΔretΔΔΔ **Struct_std_____cmd_____Cmd
return *ΔΔΔretΔΔΔ
}
func  std_____cmd_____Cmd_____exec(command string) *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_ {
_ = command
var var_3427 []string
_ = var_3427

            var_3427 = strings.Split(command, " ")

var splitted []string
_ = splitted
splitted = var_3427
var var_3428 []string
_ = var_3428

            var_3428 = splitted[1:]

var args []string
_ = args
args = var_3428
var var_3429 []string
_ = var_3429
var_3429 = Array_string_Copy((args))
var var_3430 []string
_ = var_3430
var_3430 = Array_string_Copy((splitted))
var var_3431 int
_ = var_3431
var_3431 = 0
var var_3432 string
_ = var_3432
var_3432 = var_3430[var_3431]
var var_3433 *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_3433
var_3433 = (*std_____cmd_____Cmd_____new(var_3432).args(var_3429)).run()
return var_3433
var ΔΔΔretΔΔΔ **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
return *ΔΔΔretΔΔΔ
}
func  std_____path_____FilePath_____new(p string) *Struct_std_____path_____FilePath {
_ = p
var var_3434 string
_ = var_3434

                var_3434 = filepath.Clean(p)

var var_3435 string
_ = var_3435
var_3435 = var_3434
var var_3436 Tag__none
_ = var_3436
var_3436 = Tag__none{}
var var_3437 *Struct_std_____path_____FilePath
_ = var_3437
var_3437 = &Struct_std_____path_____FilePath{path_ref: var_3435, last_error: var_3436}
return var_3437
var ΔΔΔretΔΔΔ **Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}






func  std_____col_____ArrayList_____from_array_____string(initial_elements []string) *Struct_std_____col_____ArrayList_____string {
_ = initial_elements
var var_3490 *Struct_std_____col_____ArrayList_____string
_ = var_3490
{
var var_3488 []string
_ = var_3488
var_3488 = Array_string_Copy((initial_elements))
var var_3489 *Struct_std_____col_____ArrayList_____string
_ = var_3489
var_3489 = &Struct_std_____col_____ArrayList_____string{elems: var_3488}
var_3490 = var_3489
}
return var_3490
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____string
return *ΔΔΔretΔΔΔ
}

func  std_____col_____Iter_____from_____string(next_fn func() any) *Struct_std_____col_____Iter_____string {
_ = next_fn
var var_3501 func() any
_ = var_3501
var_3501 = IDENTITY((next_fn))
var var_3502 *Struct_std_____col_____Iter_____string
_ = var_3502
var_3502 = &Struct_std_____col_____Iter_____string{next_fn: var_3501}
return var_3502
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____string
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____some_____string(value string) *Struct_std_____opt_____Opt_____string {
_ = value
var var_3503 string
_ = var_3503
var_3503 = IDENTITY((value))
var var_3504 *Struct_std_____opt_____Some_____string
_ = var_3504
var_3504 = &Struct_std_____opt_____Some_____string{value: var_3503}
var var_3505 *Struct_std_____opt_____Opt_____string
_ = var_3505
var_3505 = &Struct_std_____opt_____Opt_____string{inner: var_3504}
return var_3505
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____string
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____none_____string() *Struct_std_____opt_____Opt_____string {

var var_3506 Tag__none
_ = var_3506
var_3506 = Tag__none{}
var var_3507 *Struct_std_____opt_____Opt_____string
_ = var_3507
var_3507 = &Struct_std_____opt_____Opt_____string{inner: var_3506}
return var_3507
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____ArrayList_____new_____string() *Struct_std_____col_____ArrayList_____string {

var var_3511 *Struct_std_____col_____ArrayList_____string
_ = var_3511
{
var var_3508 []string
_ = var_3508
var_3508 = []string{}
var var_3509 []string
_ = var_3509
var_3509 = var_3508
var var_3510 *Struct_std_____col_____ArrayList_____string
_ = var_3510
var_3510 = &Struct_std_____col_____ArrayList_____string{elems: var_3509}
var_3511 = var_3510
}
return var_3511
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Ref___string(next_fn func() any) *Struct_std_____col_____Iter_____Ref___string {
_ = next_fn
var var_3512 func() any
_ = var_3512
var_3512 = IDENTITY((next_fn))
var var_3513 *Struct_std_____col_____Iter_____Ref___string
_ = var_3513
var_3513 = &Struct_std_____col_____Iter_____Ref___string{next_fn: var_3512}
return var_3513
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____RefMut___string(next_fn func() any) *Struct_std_____col_____Iter_____RefMut___string {
_ = next_fn
var var_3514 func() any
_ = var_3514
var_3514 = IDENTITY((next_fn))
var var_3515 *Struct_std_____col_____Iter_____RefMut___string
_ = var_3515
var_3515 = &Struct_std_____col_____Iter_____RefMut___string{next_fn: var_3514}
return var_3515
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____ArrayList_____new_____Ref___string() *Struct_std_____col_____ArrayList_____Ref___string {

var var_3519 *Struct_std_____col_____ArrayList_____Ref___string
_ = var_3519
{
var var_3516 []*string
_ = var_3516
var_3516 = []*string{}
var var_3517 []*string
_ = var_3517
var_3517 = var_3516
var var_3518 *Struct_std_____col_____ArrayList_____Ref___string
_ = var_3518
var_3518 = &Struct_std_____col_____ArrayList_____Ref___string{elems: var_3517}
var_3519 = var_3518
}
return var_3519
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____some_____Ref___string(value *string) *Struct_std_____opt_____Opt_____Ref___string {
_ = value
var var_3520 *string
_ = var_3520
var_3520 = IDENTITY((value))
var var_3521 *Struct_std_____opt_____Some_____Ref___string
_ = var_3521
var_3521 = &Struct_std_____opt_____Some_____Ref___string{value: var_3520}
var var_3522 *Struct_std_____opt_____Opt_____Ref___string
_ = var_3522
var_3522 = &Struct_std_____opt_____Opt_____Ref___string{inner: var_3521}
return var_3522
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____none_____Ref___string() *Struct_std_____opt_____Opt_____Ref___string {

var var_3523 Tag__none
_ = var_3523
var_3523 = Tag__none{}
var var_3524 *Struct_std_____opt_____Opt_____Ref___string
_ = var_3524
var_3524 = &Struct_std_____opt_____Opt_____Ref___string{inner: var_3523}
return var_3524
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Ref___Ref___string(next_fn func() any) *Struct_std_____col_____Iter_____Ref___Ref___string {
_ = next_fn
var var_3525 func() any
_ = var_3525
var_3525 = IDENTITY((next_fn))
var var_3526 *Struct_std_____col_____Iter_____Ref___Ref___string
_ = var_3526
var_3526 = &Struct_std_____col_____Iter_____Ref___Ref___string{next_fn: var_3525}
return var_3526
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____RefMut___Ref___string(next_fn func() any) *Struct_std_____col_____Iter_____RefMut___Ref___string {
_ = next_fn
var var_3527 func() any
_ = var_3527
var_3527 = IDENTITY((next_fn))
var var_3528 *Struct_std_____col_____Iter_____RefMut___Ref___string
_ = var_3528
var_3528 = &Struct_std_____col_____Iter_____RefMut___Ref___string{next_fn: var_3527}
return var_3528
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____empty_____Ref___string() *Struct_std_____col_____Iter_____Ref___string {

var var_3529 func() any
_ = var_3529
var_3529 = func() any {
var var_3530 Tag__no_next_elem
_ = var_3530
var_3530 = Tag__no_next_elem{}
return var_3530
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_3531 *Struct_std_____col_____Iter_____Ref___string
_ = var_3531
var_3531 = &Struct_std_____col_____Iter_____Ref___string{next_fn: var_3529}
return var_3531
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____ArrayList_____from_array_____Struct_std_____col_____Iter_____Ref___string(initial_elements []*Struct_std_____col_____Iter_____Ref___string) *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string {
_ = initial_elements
var var_3534 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = var_3534
{
var var_3532 []*Struct_std_____col_____Iter_____Ref___string
_ = var_3532
var_3532 = Array_Struct_std_____col_____Iter_____Ref___string_Copy((initial_elements))
var var_3533 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = var_3533
var_3533 = &Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string{elems: var_3532}
var_3534 = var_3533
}
return var_3534
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____some_____Struct_std_____col_____Iter_____Ref___string(value *Struct_std_____col_____Iter_____Ref___string) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string {
_ = value
var var_3535 *Struct_std_____col_____Iter_____Ref___string
_ = var_3535
var_3535 = Struct_std_____col_____Iter_____Ref___string_Copy((value))
var var_3536 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = var_3536
var_3536 = &Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string{value: var_3535}
var var_3537 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_3537
var_3537 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string{inner: var_3536}
return var_3537
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____none_____Struct_std_____col_____Iter_____Ref___string() *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string {

var var_3538 Tag__none
_ = var_3538
var_3538 = Tag__none{}
var var_3539 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_3539
var_3539 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string{inner: var_3538}
return var_3539
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____ArrayList_____new_____Struct_std_____col_____Iter_____Ref___string() *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string {

var var_3543 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = var_3543
{
var var_3540 []*Struct_std_____col_____Iter_____Ref___string
_ = var_3540
var_3540 = []*Struct_std_____col_____Iter_____Ref___string{}
var var_3541 []*Struct_std_____col_____Iter_____Ref___string
_ = var_3541
var_3541 = var_3540
var var_3542 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = var_3542
var_3542 = &Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string{elems: var_3541}
var_3543 = var_3542
}
return var_3543
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Ref___Struct_std_____col_____Iter_____Ref___string(next_fn func() any) *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___string {
_ = next_fn
var var_3544 func() any
_ = var_3544
var_3544 = IDENTITY((next_fn))
var var_3545 *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___string
_ = var_3545
var_3545 = &Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___string{next_fn: var_3544}
return var_3545
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____RefMut___Struct_std_____col_____Iter_____Ref___string(next_fn func() any) *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___string {
_ = next_fn
var var_3546 func() any
_ = var_3546
var_3546 = IDENTITY((next_fn))
var var_3547 *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___string
_ = var_3547
var_3547 = &Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___string{next_fn: var_3546}
return var_3547
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____empty_____Struct_std_____col_____Iter_____Ref___string() *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string {

var var_3548 func() any
_ = var_3548
var_3548 = func() any {
var var_3549 Tag__no_next_elem
_ = var_3549
var_3549 = Tag__no_next_elem{}
return var_3549
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_3550 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string
_ = var_3550
var_3550 = &Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string{next_fn: var_3548}
return var_3550
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Struct_std_____col_____Iter_____Ref___string(next_fn func() any) *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string {
_ = next_fn
var var_3551 func() any
_ = var_3551
var_3551 = IDENTITY((next_fn))
var var_3552 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string
_ = var_3552
var_3552 = &Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string{next_fn: var_3551}
return var_3552
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Tup_int_Ref___string(next_fn func() any) *Struct_std_____col_____Iter_____Tup_int_Ref___string {
_ = next_fn
var var_3553 func() any
_ = var_3553
var_3553 = IDENTITY((next_fn))
var var_3554 *Struct_std_____col_____Iter_____Tup_int_Ref___string
_ = var_3554
var_3554 = &Struct_std_____col_____Iter_____Tup_int_Ref___string{next_fn: var_3553}
return var_3554
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Tup_int_Ref___string
return *ΔΔΔretΔΔΔ
}
func  std_____result_____Result_____err_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_(err Tup_) *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_ {
_ = err
var var_3558 *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_3558
{
var var_3555 Tup_
var_3555 = Tup_{}
_ = var_3555
var_3555 = (err).copy()
var var_3556 *Struct_std_____result_____Err_____Tup_
_ = var_3556
var_3556 = &Struct_std_____result_____Err_____Tup_{err: var_3555}
var var_3557 *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_3557
var_3557 = &Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_{value: var_3556}
var_3558 = var_3557
}
return var_3558
var ΔΔΔretΔΔΔ **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
return *ΔΔΔretΔΔΔ
}
func  std_____result_____Result_____ok_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_(value interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}) *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_ {
_ = value
var var_3561 *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_3561
{
var var_3559 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_3559
var_3559 = IDENTITY((value))
var var_3560 *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_3560
var_3560 = &Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_{value: var_3559}
var_3561 = var_3560
}
return var_3561
var ΔΔΔretΔΔΔ **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____empty_____Struct_std_____path_____FilePath() *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var var_3562 func() any
_ = var_3562
var_3562 = func() any {
var var_3563 Tag__no_next_elem
_ = var_3563
var_3563 = Tag__no_next_elem{}
return var_3563
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_3564 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3564
var_3564 = &Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{next_fn: var_3562}
return var_3564
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____ArrayList_____new_____Struct_std_____path_____FilePath() *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath {

var var_3568 *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_3568
{
var var_3565 []*Struct_std_____path_____FilePath
_ = var_3565
var_3565 = []*Struct_std_____path_____FilePath{}
var var_3566 []*Struct_std_____path_____FilePath
_ = var_3566
var_3566 = var_3565
var var_3567 *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_3567
var_3567 = &Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath{elems: var_3566}
var_3568 = var_3567
}
return var_3568
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____some_____Struct_std_____path_____FilePath(value *Struct_std_____path_____FilePath) *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath {
_ = value
var var_3569 *Struct_std_____path_____FilePath
_ = var_3569
var_3569 = Struct_std_____path_____FilePath_Copy((value))
var var_3570 *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = var_3570
var_3570 = &Struct_std_____opt_____Some_____Struct_std_____path_____FilePath{value: var_3569}
var var_3571 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_3571
var_3571 = &Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath{inner: var_3570}
return var_3571
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____none_____Struct_std_____path_____FilePath() *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath {

var var_3572 Tag__none
_ = var_3572
var_3572 = Tag__none{}
var var_3573 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_3573
var_3573 = &Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath{inner: var_3572}
return var_3573
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Ref___Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = next_fn
var var_3574 func() any
_ = var_3574
var_3574 = IDENTITY((next_fn))
var var_3575 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3575
var_3575 = &Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{next_fn: var_3574}
return var_3575
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____RefMut___Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____RefMut___Struct_std_____path_____FilePath {
_ = next_fn
var var_3576 func() any
_ = var_3576
var_3576 = IDENTITY((next_fn))
var var_3577 *Struct_std_____col_____Iter_____RefMut___Struct_std_____path_____FilePath
_ = var_3577
var_3577 = &Struct_std_____col_____Iter_____RefMut___Struct_std_____path_____FilePath{next_fn: var_3576}
return var_3577
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____ArrayList_____new_____Ref___Struct_std_____path_____FilePath() *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath {

var var_3581 *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_3581
{
var var_3578 []**Struct_std_____path_____FilePath
_ = var_3578
var_3578 = []**Struct_std_____path_____FilePath{}
var var_3579 []**Struct_std_____path_____FilePath
_ = var_3579
var_3579 = var_3578
var var_3580 *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_3580
var_3580 = &Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath{elems: var_3579}
var_3581 = var_3580
}
return var_3581
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____some_____Ref___Struct_std_____path_____FilePath(value **Struct_std_____path_____FilePath) *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath {
_ = value
var var_3582 **Struct_std_____path_____FilePath
_ = var_3582
var_3582 = IDENTITY((value))
var var_3583 *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = var_3583
var_3583 = &Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath{value: var_3582}
var var_3584 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_3584
var_3584 = &Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath{inner: var_3583}
return var_3584
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____none_____Ref___Struct_std_____path_____FilePath() *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath {

var var_3585 Tag__none
_ = var_3585
var_3585 = Tag__none{}
var var_3586 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_3586
var_3586 = &Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath{inner: var_3585}
return var_3586
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Ref___Ref___Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____Ref___Ref___Struct_std_____path_____FilePath {
_ = next_fn
var var_3587 func() any
_ = var_3587
var_3587 = IDENTITY((next_fn))
var var_3588 *Struct_std_____col_____Iter_____Ref___Ref___Struct_std_____path_____FilePath
_ = var_3588
var_3588 = &Struct_std_____col_____Iter_____Ref___Ref___Struct_std_____path_____FilePath{next_fn: var_3587}
return var_3588
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____RefMut___Ref___Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____RefMut___Ref___Struct_std_____path_____FilePath {
_ = next_fn
var var_3589 func() any
_ = var_3589
var_3589 = IDENTITY((next_fn))
var var_3590 *Struct_std_____col_____Iter_____RefMut___Ref___Struct_std_____path_____FilePath
_ = var_3590
var_3590 = &Struct_std_____col_____Iter_____RefMut___Ref___Struct_std_____path_____FilePath{next_fn: var_3589}
return var_3590
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____empty_____Ref___Struct_std_____path_____FilePath() *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var var_3591 func() any
_ = var_3591
var_3591 = func() any {
var var_3592 Tag__no_next_elem
_ = var_3592
var_3592 = Tag__no_next_elem{}
return var_3592
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_3593 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3593
var_3593 = &Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{next_fn: var_3591}
return var_3593
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____ArrayList_____from_array_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath(initial_elements []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = initial_elements
var var_3596 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3596
{
var var_3594 []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3594
var_3594 = Array_Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((initial_elements))
var var_3595 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3595
var_3595 = &Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{elems: var_3594}
var_3596 = var_3595
}
return var_3596
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath(value *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = value
var var_3597 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3597
var_3597 = Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((value))
var var_3598 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3598
var_3598 = &Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{value: var_3597}
var var_3599 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3599
var_3599 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{inner: var_3598}
return var_3599
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____none_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath() *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var var_3600 Tag__none
_ = var_3600
var_3600 = Tag__none{}
var var_3601 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3601
var_3601 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{inner: var_3600}
return var_3601
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____ArrayList_____new_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath() *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var var_3605 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3605
{
var var_3602 []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3602
var_3602 = []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{}
var var_3603 []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3603
var_3603 = var_3602
var var_3604 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3604
var_3604 = &Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{elems: var_3603}
var_3605 = var_3604
}
return var_3605
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Ref___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = next_fn
var var_3606 func() any
_ = var_3606
var_3606 = IDENTITY((next_fn))
var var_3607 *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3607
var_3607 = &Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{next_fn: var_3606}
return var_3607
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____RefMut___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = next_fn
var var_3608 func() any
_ = var_3608
var_3608 = IDENTITY((next_fn))
var var_3609 *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3609
var_3609 = &Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{next_fn: var_3608}
return var_3609
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____empty_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath() *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var var_3610 func() any
_ = var_3610
var_3610 = func() any {
var var_3611 Tag__no_next_elem
_ = var_3611
var_3611 = Tag__no_next_elem{}
return var_3611
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_3612 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3612
var_3612 = &Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{next_fn: var_3610}
return var_3612
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = next_fn
var var_3613 func() any
_ = var_3613
var_3613 = IDENTITY((next_fn))
var var_3614 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_3614
var_3614 = &Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{next_fn: var_3613}
return var_3614
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Tup_int_Ref___Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____Tup_int_Ref___Struct_std_____path_____FilePath {
_ = next_fn
var var_3615 func() any
_ = var_3615
var_3615 = IDENTITY((next_fn))
var var_3616 *Struct_std_____col_____Iter_____Tup_int_Ref___Struct_std_____path_____FilePath
_ = var_3616
var_3616 = &Struct_std_____col_____Iter_____Tup_int_Ref___Struct_std_____path_____FilePath{next_fn: var_3615}
return var_3616
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Tup_int_Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = next_fn
var var_3617 func() any
_ = var_3617
var_3617 = IDENTITY((next_fn))
var var_3618 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3618
var_3618 = &Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{next_fn: var_3617}
return var_3618
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____empty_____string() *Struct_std_____col_____Iter_____string {

var var_3619 func() any
_ = var_3619
var_3619 = func() any {
var var_3620 Tag__no_next_elem
_ = var_3620
var_3620 = Tag__no_next_elem{}
return var_3620
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_3621 *Struct_std_____col_____Iter_____string
_ = var_3621
var_3621 = &Struct_std_____col_____Iter_____string{next_fn: var_3619}
return var_3621
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____string
return *ΔΔΔretΔΔΔ
}


func  std_____col_____ArrayList_____new_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath() *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {

var var_3651 *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_3651
{
var var_3648 []interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_3648
var_3648 = []interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}{}
var var_3649 []interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_3649
var_3649 = var_3648
var var_3650 *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_3650
var_3650 = &Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath{elems: var_3649}
var_3651 = var_3650
}
return var_3651
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____ArrayList_____from_array_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath(initial_elements []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = initial_elements
var var_3654 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3654
{
var var_3652 []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3652
var_3652 = Array_Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((initial_elements))
var var_3653 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3653
var_3653 = &Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{elems: var_3652}
var_3654 = var_3653
}
return var_3654
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath(value *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = value
var var_3655 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3655
var_3655 = Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((value))
var var_3656 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3656
var_3656 = &Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{value: var_3655}
var var_3657 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3657
var_3657 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{inner: var_3656}
return var_3657
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____none_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath() *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var var_3658 Tag__none
_ = var_3658
var_3658 = Tag__none{}
var var_3659 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3659
var_3659 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{inner: var_3658}
return var_3659
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____ArrayList_____new_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath() *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var var_3663 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3663
{
var var_3660 []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3660
var_3660 = []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{}
var var_3661 []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3661
var_3661 = var_3660
var var_3662 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3662
var_3662 = &Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{elems: var_3661}
var_3663 = var_3662
}
return var_3663
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Ref___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = next_fn
var var_3664 func() any
_ = var_3664
var_3664 = IDENTITY((next_fn))
var var_3665 *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3665
var_3665 = &Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{next_fn: var_3664}
return var_3665
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____RefMut___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = next_fn
var var_3666 func() any
_ = var_3666
var_3666 = IDENTITY((next_fn))
var var_3667 *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3667
var_3667 = &Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{next_fn: var_3666}
return var_3667
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____empty_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath() *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var var_3668 func() any
_ = var_3668
var_3668 = func() any {
var var_3669 Tag__no_next_elem
_ = var_3669
var_3669 = Tag__no_next_elem{}
return var_3669
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_3670 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3670
var_3670 = &Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{next_fn: var_3668}
return var_3670
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = next_fn
var var_3671 func() any
_ = var_3671
var_3671 = IDENTITY((next_fn))
var var_3672 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_3672
var_3672 = &Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{next_fn: var_3671}
return var_3672
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Tup_int_Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____Tup_int_Struct_std_____path_____FilePath {
_ = next_fn
var var_3673 func() any
_ = var_3673
var_3673 = IDENTITY((next_fn))
var var_3674 *Struct_std_____col_____Iter_____Tup_int_Struct_std_____path_____FilePath
_ = var_3674
var_3674 = &Struct_std_____col_____Iter_____Tup_int_Struct_std_____path_____FilePath{next_fn: var_3673}
return var_3674
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Tup_int_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____some_____Duck_exitcode_int_stderr_string_stdout_string(value interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}) *Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string {
_ = value
var var_3675 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_3675
var_3675 = IDENTITY((value))
var var_3676 *Struct_std_____opt_____Some_____Duck_exitcode_int_stderr_string_stdout_string
_ = var_3676
var_3676 = &Struct_std_____opt_____Some_____Duck_exitcode_int_stderr_string_stdout_string{value: var_3675}
var var_3677 *Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string
_ = var_3677
var_3677 = &Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string{inner: var_3676}
return var_3677
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____none_____Duck_exitcode_int_stderr_string_stdout_string() *Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string {

var var_3678 Tag__none
_ = var_3678
var_3678 = Tag__none{}
var var_3679 *Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string
_ = var_3679
var_3679 = &Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string{inner: var_3678}
return var_3679
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____some_____Tup_(value Tup_) *Struct_std_____opt_____Opt_____Tup_ {
_ = value
var var_3680 Tup_
var_3680 = Tup_{}
_ = var_3680
var_3680 = (value).copy()
var var_3681 *Struct_std_____opt_____Some_____Tup_
_ = var_3681
var_3681 = &Struct_std_____opt_____Some_____Tup_{value: var_3680}
var var_3682 *Struct_std_____opt_____Opt_____Tup_
_ = var_3682
var_3682 = &Struct_std_____opt_____Opt_____Tup_{inner: var_3681}
return var_3682
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Tup_
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____none_____Tup_() *Struct_std_____opt_____Opt_____Tup_ {

var var_3683 Tag__none
_ = var_3683
var_3683 = Tag__none{}
var var_3684 *Struct_std_____opt_____Opt_____Tup_
_ = var_3684
var_3684 = &Struct_std_____opt_____Opt_____Tup_{inner: var_3683}
return var_3684
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Tup_
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____some_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath(value interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}) *Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {
_ = value
var var_3685 interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_3685
var_3685 = IDENTITY((value))
var var_3686 *Struct_std_____opt_____Some_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_3686
var_3686 = &Struct_std_____opt_____Some_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath{value: var_3685}
var var_3687 *Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_3687
var_3687 = &Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath{inner: var_3686}
return var_3687
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____opt_____Opt_____none_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath() *Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {

var var_3688 Tag__none
_ = var_3688
var_3688 = Tag__none{}
var var_3689 *Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_3689
var_3689 = &Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath{inner: var_3688}
return var_3689
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____Ref___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____Ref___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {
_ = next_fn
var var_3690 func() any
_ = var_3690
var_3690 = IDENTITY((next_fn))
var var_3691 *Struct_std_____col_____Iter_____Ref___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_3691
var_3691 = &Struct_std_____col_____Iter_____Ref___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath{next_fn: var_3690}
return var_3691
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  std_____col_____Iter_____from_____RefMut___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath(next_fn func() any) *Struct_std_____col_____Iter_____RefMut___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {
_ = next_fn
var var_3692 func() any
_ = var_3692
var_3692 = IDENTITY((next_fn))
var var_3693 *Struct_std_____col_____Iter_____RefMut___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_3693
var_3693 = &Struct_std_____col_____Iter_____RefMut___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath{next_fn: var_3692}
return var_3693
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

type RenderCall struct {
Jsx string
Id string
}
type TemplEnv struct {
ClientComponents []string
RenderCalls []RenderCall
}





func  Extend_string_with_starts_with(self string) func (string) bool {
_ = self
return func(prefix string) bool {
var var_3716 bool
_ = var_3716
var_3716 = false
var res bool
_ = res
res = var_3716
var var_3717 Tup_
var_3717 = Tup_{}
_ = var_3717

            res = strings.HasPrefix(self, prefix)

var var_3718 bool
_ = var_3718
var_3718 = IDENTITY((res))
return var_3718
}
}
func  Extend_string_with_ends_with(self string) func (string) bool {
_ = self
return func(suffix string) bool {
var var_3719 bool
_ = var_3719
var_3719 = false
var res bool
_ = res
res = var_3719
var var_3720 Tup_
var_3720 = Tup_{}
_ = var_3720

            res = strings.HasSuffix(self, suffix)

var var_3721 bool
_ = var_3721
var_3721 = IDENTITY((res))
return var_3721
}
}


func  Extend_string_with_trim_prefix(self string) func (string) string {
_ = self
return func(prefix string) string {
var var_3742 string
_ = var_3742

            var_3742 = strings.TrimPrefix(self, prefix)

var var_3743 string
_ = var_3743
var_3743 = var_3742
return var_3743
}
}

func  Extend_string_with_replace(self string) func (string,string) string {
_ = self
return func(old_s string, new_s string) string {
var var_3749 string
_ = var_3749
var_3749 = ""
var s string
_ = s
s = var_3749
var var_3750 Tup_
var_3750 = Tup_{}
_ = var_3750

            s = strings.ReplaceAll(self, old_s, new_s)

var var_3751 string
_ = var_3751
var_3751 = IDENTITY((s))
return var_3751
}
}












func  Array_Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath_Copy(self []interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}) []interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
} {
_ = self

                        res := make([]interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}, len(self))

                        for i := range self {
                            a := self[i]
                            _ = a
                            a_x := IDENTITY((a))
                            res[i] = a_x
                        }

                        return res

}

func  Array_Ref___Struct_std_____path_____FilePath_Copy(self []**Struct_std_____path_____FilePath) []**Struct_std_____path_____FilePath {
_ = self

                        res := make([]**Struct_std_____path_____FilePath, len(self))

                        for i := range self {
                            a := self[i]
                            _ = a
                            a_x := IDENTITY((a))
                            res[i] = a_x
                        }

                        return res

}



func  Array_Ref___string_Copy(self []*string) []*string {
_ = self

                        res := make([]*string, len(self))

                        for i := range self {
                            a := self[i]
                            _ = a
                            a_x := IDENTITY((a))
                            res[i] = a_x
                        }

                        return res

}


func  Array_Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy(self []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = self

                        res := make([]*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath, len(self))

                        for i := range self {
                            a := self[i]
                            _ = a
                            a_x := Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((a))
                            res[i] = a_x
                        }

                        return res

}

func  Array_Struct_std_____col_____Iter_____Ref___string_Copy(self []*Struct_std_____col_____Iter_____Ref___string) []*Struct_std_____col_____Iter_____Ref___string {
_ = self

                        res := make([]*Struct_std_____col_____Iter_____Ref___string, len(self))

                        for i := range self {
                            a := self[i]
                            _ = a
                            a_x := Struct_std_____col_____Iter_____Ref___string_Copy((a))
                            res[i] = a_x
                        }

                        return res

}

func  Array_Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy(self []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = self

                        res := make([]*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath, len(self))

                        for i := range self {
                            a := self[i]
                            _ = a
                            a_x := Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((a))
                            res[i] = a_x
                        }

                        return res

}

func  Array_Struct_std_____path_____FilePath_Copy(self []*Struct_std_____path_____FilePath) []*Struct_std_____path_____FilePath {
_ = self

                        res := make([]*Struct_std_____path_____FilePath, len(self))

                        for i := range self {
                            a := self[i]
                            _ = a
                            a_x := Struct_std_____path_____FilePath_Copy((a))
                            res[i] = a_x
                        }

                        return res

}









func  Array_string_Iter(self []string) *Struct_std_____col_____Iter_____Ref___string {
_ = self

                        idx := 0
                        f := func() any {
                            if idx >= len(self) {
                                return Tag__no_next_elem{}
                            }

                            elem_to_ret := &self[idx]
                            idx = idx + 1
                            return elem_to_ret
                        }

                        return std_____col_____Iter_____from_____Ref___string(f)

}



func  Array_string_Copy(self []string) []string {
_ = self

                        res := make([]string, len(self))

                        for i := range self {
                            a := self[i]
                            _ = a
                            a_x := IDENTITY((a))
                            res[i] = a_x
                        }

                        return res

}

type Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath struct {
binary_output interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
dargo_output interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
src_path *Struct_std_____path_____FilePath
}
type Hasbinary_output[T any] interface {
Getbinary_output() T
GetPtrbinary_output() *T
Setbinary_output(param T)
}
func (self *Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) Getbinary_output() interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
} {

return self.binary_output
}
func (self *Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) GetPtrbinary_output() *interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
} {

return &self.binary_output
}
func (self *Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) Setbinary_output(param interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
})  {
_ = param
self.binary_output = param
}
type Hasdargo_output[T any] interface {
Getdargo_output() T
GetPtrdargo_output() *T
Setdargo_output(param T)
}
func (self *Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) Getdargo_output() interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
} {

return self.dargo_output
}
func (self *Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) GetPtrdargo_output() *interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
} {

return &self.dargo_output
}
func (self *Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) Setdargo_output(param interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
})  {
_ = param
self.dargo_output = param
}
type Hassrc_path[T any] interface {
Getsrc_path() T
GetPtrsrc_path() *T
Setsrc_path(param T)
}
func (self *Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) Getsrc_path() *Struct_std_____path_____FilePath {

return self.src_path
}
func (self *Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) GetPtrsrc_path() **Struct_std_____path_____FilePath {

return &self.src_path
}
func (self *Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) Setsrc_path(param *Struct_std_____path_____FilePath)  {
_ = param
self.src_path = param
}













































type Duck_exitcode_int_stderr_string_stdout_string struct {
exitcode int
stderr string
stdout string
}
type Hasexitcode[T any] interface {
Getexitcode() T
GetPtrexitcode() *T
Setexitcode(param T)
}
func (self *Duck_exitcode_int_stderr_string_stdout_string) Getexitcode() int {

return self.exitcode
}
func (self *Duck_exitcode_int_stderr_string_stdout_string) GetPtrexitcode() *int {

return &self.exitcode
}
func (self *Duck_exitcode_int_stderr_string_stdout_string) Setexitcode(param int)  {
_ = param
self.exitcode = param
}
type Hasstderr[T any] interface {
Getstderr() T
GetPtrstderr() *T
Setstderr(param T)
}
func (self *Duck_exitcode_int_stderr_string_stdout_string) Getstderr() string {

return self.stderr
}
func (self *Duck_exitcode_int_stderr_string_stdout_string) GetPtrstderr() *string {

return &self.stderr
}
func (self *Duck_exitcode_int_stderr_string_stdout_string) Setstderr(param string)  {
_ = param
self.stderr = param
}
type Hasstdout[T any] interface {
Getstdout() T
GetPtrstdout() *T
Setstdout(param T)
}
func (self *Duck_exitcode_int_stderr_string_stdout_string) Getstdout() string {

return self.stdout
}
func (self *Duck_exitcode_int_stderr_string_stdout_string) GetPtrstdout() *string {

return &self.stdout
}
func (self *Duck_exitcode_int_stderr_string_stdout_string) Setstdout(param string)  {
_ = param
self.stdout = param
}










































type Tag__default struct {

}

type Tag__equal struct {

}

type Tag__err struct {

}

type Tag__greater struct {

}

type Tag__lax struct {

}

type Tag__no_next_elem struct {

}

type Tag__none struct {

}


type Tag__smaller struct {

}

type Tag__strict struct {

}

type Tup_ struct {

}


func (self Tup_) copy() Tup_ {

res := *new(Tup_)
return res
}

type Tup_int_Ref___Struct_std_____path_____FilePath struct {
field_0 int
field_1 **Struct_std_____path_____FilePath
}
func (self Tup_int_Ref___Struct_std_____path_____FilePath) copy() Tup_int_Ref___Struct_std_____path_____FilePath {

res := *new(Tup_int_Ref___Struct_std_____path_____FilePath)
res.field_0 = (IDENTITY((self.field_0)))
res.field_1 = (IDENTITY((self.field_1)))
return res
}
type Tup_int_Ref___string struct {
field_0 int
field_1 *string
}


func (self Tup_int_Ref___string) copy() Tup_int_Ref___string {

res := *new(Tup_int_Ref___string)
res.field_0 = (IDENTITY((self.field_0)))
res.field_1 = (IDENTITY((self.field_1)))
return res
}

type Tup_int_Struct_std_____path_____FilePath struct {
field_0 int
field_1 *Struct_std_____path_____FilePath
}
func (self Tup_int_Struct_std_____path_____FilePath) copy() Tup_int_Struct_std_____path_____FilePath {

res := *new(Tup_int_Struct_std_____path_____FilePath)
res.field_0 = (IDENTITY((self.field_0)))
res.field_1 = (Struct_std_____path_____FilePath_Copy((self.field_1)))
return res
}
type Tup_string_string struct {
field_0 string
field_1 string
}


func (self Tup_string_string) copy() Tup_string_string {

res := *new(Tup_string_string)
res.field_0 = (IDENTITY((self.field_0)))
res.field_1 = (IDENTITY((self.field_1)))
return res
}

































type Struct_std_____time_____Time struct {
raw_time time.Time
}





































































func  Struct_std_____time_____Time_Copy(self *Struct_std_____time_____Time) *Struct_std_____time_____Time {
_ = self
return &Struct_std_____time_____Time{
raw_time: IDENTITY((self.raw_time)),
}

}



















type Struct_std_____web_____Req struct {
request_ref *http.Request
}



func (duck_internal_self *Struct_std_____web_____Req) header(name string) any {
_ = name
var self **Struct_std_____web_____Req
_ = self
self = &duck_internal_self
var var_215 Tup_
var_215 = Tup_{}
_ = var_215

            value := (*self).request_ref.Header.Get(name)
            if value == "" {
            	return Tag__none {}
            }

            return value;

var var_216 Tag__none
_ = var_216
var_216 = Tag__none{}
return var_216
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____web_____Req) param(name string) any {
_ = name
var self **Struct_std_____web_____Req
_ = self
self = &duck_internal_self
var var_217 string
_ = var_217

            value := (*self).request_ref.PathValue(name)
            if value == "" {
                return Tag__none {}
            }
            var_217 = value

var var_218 string
_ = var_218
var_218 = var_217
return var_218
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____web_____Req) query_param(name string) any {
_ = name
var self **Struct_std_____web_____Req
_ = self
self = &duck_internal_self
var var_220 Tup_
var_220 = Tup_{}
_ = var_220
{
var var_219 Tup_
var_219 = Tup_{}
_ = var_219

            value := (*self).request_ref.URL.Query().Get(name)
            if value == "" {
                return Tag__none { }
            }
            return value

var_220 = var_219
}
return var_220
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____web_____Req) body_str() any {

var self **Struct_std_____web_____Req
_ = self
self = &duck_internal_self
var var_221 string
_ = var_221

            bodyBytes, err := ioutil.ReadAll((*self).request_ref.Body)
            if err != nil {
            	return Tag__err { }
            }

            (*self).request_ref.Body = ioutil.NopCloser(bytes.NewBuffer(bodyBytes))
            var_221 = string(bodyBytes)

var var_222 string
_ = var_222
var_222 = var_221
return var_222
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____web_____Req) cookie(name string) any {
_ = name
var self **Struct_std_____web_____Req
_ = self
self = &duck_internal_self
var var_223 *http.Cookie
_ = var_223

            var_223, err := (*self).request_ref.Cookie(name)
            if err != nil {
            	return Tag__none {}
            }

var cookie *http.Cookie
_ = cookie
cookie = var_223
var var_224 *http.Cookie
_ = var_224
var_224 = IDENTITY((cookie))
var var_225 *Struct_std_____web_____Cookie
_ = var_225
var_225 = std_____web_____Cookie_____from_go_cookie(var_224)
return var_225
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

type Struct_std_____web_____Cookie struct {
fname string
fvalue string
fquoted bool
fpath string
fdomain string
fexpires any
fraw_expires string
fmax_age int
fsecure bool
fhttp_only bool
fsame_site any
fpartitioned bool
fraw string
funparsed []string
}




















func (duck_internal_self *Struct_std_____web_____Cookie) to_go_cookie() *http.Cookie {

var self **Struct_std_____web_____Cookie
_ = self
self = &duck_internal_self
var var_226 time.Time
_ = var_226

            if ((*self).fexpires == Tag__none {}) {
            	var_226 = time.Time {}
            } else {
            	t, okkk := (*self).fexpires.(time.Time);
             	if okkk {
             		var_226 = t
              	} else {
               		var_226 = time.Time {}
               	}
            }

var expires time.Time
_ = expires
expires = var_226
var var_227 **Struct_std_____web_____Cookie
_ = var_227
var_227 = IDENTITY((self))
var var_228 http.SameSite
_ = var_228

var var_229 any
_ = var_229
var_229 = (*var_227).fsame_site
if var_231, ok := var_229.(Tag__default); ok {
_ = var_231

if true {
var var_232 http.SameSite
_ = var_232
 var_232 = http.SameSiteDefaultMode
var var_233 http.SameSite
_ = var_233
var_233 = var_232
var_228 = var_233
goto var_230

}
}
if var_234, ok := var_229.(Tag__lax); ok {
_ = var_234

if true {
var var_235 http.SameSite
_ = var_235
 var_235 = http.SameSiteLaxMode
var var_236 http.SameSite
_ = var_236
var_236 = var_235
var_228 = var_236
goto var_230

}
}
if var_237, ok := var_229.(Tag__strict); ok {
_ = var_237

if true {
var var_238 http.SameSite
_ = var_238
 var_238 = http.SameSiteStrictMode
var var_239 http.SameSite
_ = var_239
var_239 = var_238
var_228 = var_239
goto var_230

}
}
if var_240, ok := var_229.(Tag__none); ok {
_ = var_240

if true {
var var_241 http.SameSite
_ = var_241
 var_241 = http.SameSiteNoneMode
var var_242 http.SameSite
_ = var_242
var_242 = var_241
var_228 = var_242
goto var_230

}
}

                    var_230:
                    if false {}

var var_243 http.SameSite
_ = var_243
var_243 = var_228
var same_site http.SameSite
_ = same_site
same_site = var_243
var var_244 http.Cookie
_ = var_244

            cookie := http.Cookie {}
            cookie.Name = (*self).fname;
            cookie.Value = (*self).fvalue;
            cookie.Path = (*self).fpath;
            cookie.Domain = (*self).fdomain;
            cookie.Expires = expires;
            cookie.RawExpires = (*self).fraw_expires;
            cookie.MaxAge = (*self).fmax_age;
            cookie.Secure = (*self).fsecure;
            cookie.HttpOnly = (*self).fhttp_only;
            cookie.SameSite = same_site;
            cookie.Partitioned = (*self).fpartitioned;
            cookie.Raw = (*self).fraw;
            cookie.Unparsed = (*self).funparsed;

            var_244 = cookie

var var_245 http.Cookie
_ = var_245
var_245 = var_244
var go_cookie http.Cookie
_ = go_cookie
go_cookie = var_245
var var_246 *http.Cookie
_ = var_246
var_246 = &go_cookie
return var_246
var ΔΔΔretΔΔΔ **http.Cookie
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____web_____Cookie) http_only(enabled bool) **Struct_std_____web_____Cookie {
_ = enabled
var Δorg_addr *Struct_std_____web_____Cookie
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____web_____Cookie
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_247 bool
_ = var_247
var_247 = IDENTITY((enabled))
(*self).fhttp_only = var_247
var var_248 **Struct_std_____web_____Cookie
_ = var_248
var_248 = IDENTITY((self))
return var_248
var ΔΔΔretΔΔΔ ***Struct_std_____web_____Cookie
return *ΔΔΔretΔΔΔ
}

type Struct_std_____web_____Res struct {
writer *http.ResponseWriter
}



func (duck_internal_self *Struct_std_____web_____Res) status(code int) **Struct_std_____web_____Res {
_ = code
var self **Struct_std_____web_____Res
_ = self
self = &duck_internal_self
var var_249 Tup_
var_249 = Tup_{}
_ = var_249

            (*(*self).writer).WriteHeader(int(code))

var var_250 **Struct_std_____web_____Res
_ = var_250
var_250 = IDENTITY((self))
return var_250
var ΔΔΔretΔΔΔ ***Struct_std_____web_____Res
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____web_____Res) header(key string, value string) **Struct_std_____web_____Res {
_ = key
_ = value
var self **Struct_std_____web_____Res
_ = self
self = &duck_internal_self
var var_251 Tup_
var_251 = Tup_{}
_ = var_251

            (*(*self).writer).Header().Set(key, value)

var var_252 **Struct_std_____web_____Res
_ = var_252
var_252 = IDENTITY((self))
return var_252
var ΔΔΔretΔΔΔ ***Struct_std_____web_____Res
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____web_____Res) set_cookie(cookie **Struct_std_____web_____Cookie) **Struct_std_____web_____Res {
_ = cookie
var Δorg_addr *Struct_std_____web_____Res
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____web_____Res
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var go_cookie *http.Cookie
_ = go_cookie
go_cookie = (*cookie).to_go_cookie()
var var_253 Tup_
var_253 = Tup_{}
_ = var_253

            http.SetCookie(*(*self).writer, go_cookie)

var var_254 **Struct_std_____web_____Res
_ = var_254
var_254 = IDENTITY((self))
return var_254
var ΔΔΔretΔΔΔ ***Struct_std_____web_____Res
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____web_____Res) str(content string) Tup_ {
_ = content
var self **Struct_std_____web_____Res
_ = self
self = &duck_internal_self
var var_256 Tup_
var_256 = Tup_{}
_ = var_256
{
var var_255 Tup_
var_255 = Tup_{}
_ = var_255

            (*(*self).writer).Write([]byte(content))

var_256 = var_255
}
return var_256
return Tup_{}
}

func (duck_internal_self *Struct_std_____web_____Res) json_str(str string) Tup_ {
_ = str
var self **Struct_std_____web_____Res
_ = self
self = &duck_internal_self
var var_262 Tup_
var_262 = Tup_{}
_ = var_262
{
var var_257 string
_ = var_257
var_257 = IDENTITY((str))
var var_258 string
_ = var_258
var_258 = "Content-Type"
var var_259 string
_ = var_259
var_259 = "application/json"
var var_260 Tup_
var_260 = Tup_{}
_ = var_260
var_260 = (*(*self).header(var_258, var_259)).str(var_257)
var var_261 Tup_
var_261 = Tup_{}
_ = var_261
var_261 = Tup_{}
var_262 = var_261
}
return var_262
return Tup_{}
}

func (duck_internal_self *Struct_std_____web_____Res) duckx(templ func (env *TemplEnv) string) Tup_ {
_ = templ
var self **Struct_std_____web_____Res
_ = self
self = &duck_internal_self
var var_266 Tup_
var_266 = Tup_{}
_ = var_266
{
var var_263 func (env *TemplEnv) string
_ = var_263
var_263 = IDENTITY((templ))
var var_264 string
_ = var_264
var_264 = std_____web_____render(var_263)
var var_265 Tup_
var_265 = Tup_{}
_ = var_265
var_265 = (*self).str(var_264)
var_266 = var_265
}
return var_266
return Tup_{}
}















type Struct_std_____sync_____AtomicBool struct {
inner atomic.Bool
}



func (duck_internal_self *Struct_std_____sync_____AtomicBool) compare_and_swap(compare_against bool, new_value bool) bool {
_ = compare_against
_ = new_value
var self **Struct_std_____sync_____AtomicBool
_ = self
self = &duck_internal_self
var var_341 bool
_ = var_341
{
var var_339 bool
_ = var_339

            var_339 = (*self).inner.CompareAndSwap(compare_against, new_value)

var var_340 bool
_ = var_340
var_340 = var_339
var_341 = var_340
}
return var_341
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____sync_____AtomicBool) set(new_value bool) bool {
_ = new_value
var self **Struct_std_____sync_____AtomicBool
_ = self
self = &duck_internal_self
var var_344 bool
_ = var_344
{
var var_342 bool
_ = var_342

            var_342 = (*self).inner.Swap(new_value)

var var_343 bool
_ = var_343
var_343 = var_342
var_344 = var_343
}
return var_344
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____sync_____AtomicBool) get() bool {

var self **Struct_std_____sync_____AtomicBool
_ = self
self = &duck_internal_self
var var_347 bool
_ = var_347
{
var var_345 bool
_ = var_345

            var_345 = (*self).inner.Load()

var var_346 bool
_ = var_346
var_346 = var_345
var_347 = var_346
}
return var_347
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

type Struct_std_____cmd_____Process struct {
inner *os.Process
}



type Struct_std_____cmd_____Cmd struct {
program string
cmd_args *Struct_std_____col_____ArrayList_____string
cwd any
}





func (duck_internal_self *Struct_std_____cmd_____Cmd) dir(p string) **Struct_std_____cmd_____Cmd {
_ = p
var Δorg_addr *Struct_std_____cmd_____Cmd
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____cmd_____Cmd
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_350 string
_ = var_350
var_350 = IDENTITY((p))
(*self).cwd = var_350
var var_351 **Struct_std_____cmd_____Cmd
_ = var_351
var_351 = IDENTITY((self))
return var_351
var ΔΔΔretΔΔΔ ***Struct_std_____cmd_____Cmd
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____cmd_____Cmd) arg(arg string) **Struct_std_____cmd_____Cmd {
_ = arg
var Δorg_addr *Struct_std_____cmd_____Cmd
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____cmd_____Cmd
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_352 string
_ = var_352
var_352 = IDENTITY((arg))
var var_353 **Struct_std_____col_____ArrayList_____string
_ = var_353
var_353 = (*self).cmd_args.push(var_352)
var var_354 **Struct_std_____cmd_____Cmd
_ = var_354
var_354 = IDENTITY((self))
return var_354
var ΔΔΔretΔΔΔ ***Struct_std_____cmd_____Cmd
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____cmd_____Cmd) args(_args []string) **Struct_std_____cmd_____Cmd {
_ = _args
var Δorg_addr *Struct_std_____cmd_____Cmd
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____cmd_____Cmd
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_355 *Struct_std_____col_____Iter_____Ref___string
_ = var_355
var_355 = Array_string_Iter(_args)
if false {goto label_356}
label_356:
for {
var arg *string
_ = arg
var var_362 any
_ = var_362
var_362 = var_355.next()
switch (var_362).(type) {
case *string: {
var var_363 *string
_ = var_363
var_363 = var_362.(*string)
arg = var_363
break
}
case Tag__no_next_elem: {
break label_356
break
}
}
var var_361 Tup_
var_361 = Tup_{}
_ = var_361
{
var var_357 *string
_ = var_357
var_357 = IDENTITY((arg))
var var_358 string
_ = var_358
var_358 = *var_357
var var_359 **Struct_std_____col_____ArrayList_____string
_ = var_359
var_359 = (*self).cmd_args.push(var_358)
var var_360 Tup_
var_360 = Tup_{}
_ = var_360
var_360 = Tup_{}
var_361 = var_360
}
}
var var_364 **Struct_std_____cmd_____Cmd
_ = var_364
var_364 = IDENTITY((self))
return var_364
var ΔΔΔretΔΔΔ ***Struct_std_____cmd_____Cmd
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____cmd_____Cmd) spawn() any {

var self **Struct_std_____cmd_____Cmd
_ = self
self = &duck_internal_self
var var_365 *os.Process
_ = var_365

            cmd := exec.Command((*self).program, (*self).cmd_args.elems...)
            if dir, ok := (*self).cwd.(string); ok {
            	cmd.Dir = dir
            }

            err := cmd.Start()
            if err != nil {
            	return Tag__err {}
            }

            var_365 = cmd.Process

var process *os.Process
_ = process
process = var_365
var var_366 *os.Process
_ = var_366
var_366 = IDENTITY((process))
var var_367 *Struct_std_____cmd_____Process
_ = var_367
var_367 = &Struct_std_____cmd_____Process{inner: var_366}
return var_367
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____cmd_____Cmd) run() *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_ {

var self **Struct_std_____cmd_____Cmd
_ = self
self = &duck_internal_self
var var_368 int
_ = var_368
var_368 = 0
var var_369 string
_ = var_369
var_369 = ""
var var_370 string
_ = var_370
var_370 = ""
var var_371 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_371
var_371 = &Duck_exitcode_int_stderr_string_stdout_string{exitcode: var_368, stderr: var_369, stdout: var_370}
var output interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = output
output = var_371
var var_372 Tup_
var_372 = Tup_{}
_ = var_372
var_372 = Tup_{}
var err_res *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = err_res
err_res = std_____result_____Result_____err_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_(var_372)
var var_373 Tup_
var_373 = Tup_{}
_ = var_373

            cmd := exec.Command((*self).program, (*self).cmd_args.elems...)

            if dir, ok := (*self).cwd.(string); ok {
            	cmd.Dir = dir
            }

            var outBuf bytes.Buffer
            var errBuf bytes.Buffer

            cmd.Stdout = &outBuf;
            cmd.Stderr = &errBuf;

            err := cmd.Run()
            if err != nil {
                if exitErr, ok := err.(*exec.ExitError); ok {
                    output.Setstderr(string(exitErr.Stderr))
                    output.Setexitcode(exitErr.ExitCode())
                    goto err_program_exited_017171
                }

                // todo: return go error
                return err_res
            }

            output.Setstdout(outBuf.String())
            output.Setstderr(errBuf.String())
            err_program_exited_017171:

var var_374 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_374
var_374 = IDENTITY((output))
var var_375 *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_375
var_375 = std_____result_____Result_____ok_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_(var_374)
return var_375
var ΔΔΔretΔΔΔ **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____cmd_____Cmd) run_and_forget() any {

var self **Struct_std_____cmd_____Cmd
_ = self
self = &duck_internal_self
var var_377 Tup_
var_377 = Tup_{}
_ = var_377
{
var var_376 Tup_
var_376 = Tup_{}
_ = var_376

            cmd := exec.Command((*self).program, (*self).cmd_args.elems...)

            if dir, ok := (*self).cwd.(string); ok {
               	cmd.Dir = dir
            }

            err := cmd.Run()
            if err != nil {
                return Tag__err { }
            }

            return Tag__none {}

var_377 = var_376
}
return var_377
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}




type Struct_std_____path_____FilePath struct {
path_ref string
last_error any
}




func (duck_internal_self *Struct_std_____path_____FilePath) iter_dirs() *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var empty_iter *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = empty_iter
empty_iter = std_____col_____Iter_____empty_____Struct_std_____path_____FilePath()
var var_378 bool
_ = var_378
var_378 = (*self).failed()
var var_379 Tup_
var_379 = Tup_{}
_ = var_379
if var_378 {
var var_380 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_380
var_380 = Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((empty_iter))
return var_380
}
var entries *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = entries
entries = std_____col_____ArrayList_____new_____Struct_std_____path_____FilePath()
var var_381 func(path_ref string) *Struct_std_____path_____FilePath
_ = var_381
var_381 = func(path_ref string) *Struct_std_____path_____FilePath {
var var_385 *Struct_std_____path_____FilePath
_ = var_385
{
var var_382 string
_ = var_382
var_382 = IDENTITY((path_ref))
var var_383 Tag__none
_ = var_383
var_383 = Tag__none{}
var var_384 *Struct_std_____path_____FilePath
_ = var_384
var_384 = &Struct_std_____path_____FilePath{path_ref: var_382, last_error: var_383}
var_385 = var_384
}
return var_385
var ΔΔΔretΔΔΔ **Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
var to_dir_entry func(path_ref string) *Struct_std_____path_____FilePath
_ = to_dir_entry
to_dir_entry = var_381
var var_386 bool
_ = var_386
var_386 = false
var error bool
_ = error
error = var_386
var var_387 Tup_
var_387 = Tup_{}
_ = var_387

            files, err := os.ReadDir((*self).path_ref)
            if err != nil {
                (*self).last_error = err.Error()
                return empty_iter
            }

            for _, f := range files {
                child_path := filepath.Join((*self).path_ref, f.Name())
                entries.push(to_dir_entry(child_path))
            }

var var_388 func(entry **Struct_std_____path_____FilePath) *Struct_std_____path_____FilePath
_ = var_388
var_388 = func(entry **Struct_std_____path_____FilePath) *Struct_std_____path_____FilePath {
var var_391 *Struct_std_____path_____FilePath
_ = var_391
{
var var_389 **Struct_std_____path_____FilePath
_ = var_389
var_389 = IDENTITY((entry))
var var_390 *Struct_std_____path_____FilePath
_ = var_390
var_390 = *var_389
var_391 = var_390
}
return var_391
var ΔΔΔretΔΔΔ **Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
var var_392 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_392
var_392 = entries.iter().map_____Struct_std_____path_____FilePath(var_388)
return var_392
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) walk() *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_393 bool
_ = var_393
var_393 = (*self).failed()
var var_394 Tup_
var_394 = Tup_{}
_ = var_394
if var_393 {
var var_395 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_395
var_395 = std_____col_____Iter_____empty_____Struct_std_____path_____FilePath()
return var_395
}
var file_paths *Struct_std_____col_____ArrayList_____string
_ = file_paths
file_paths = std_____col_____ArrayList_____new_____string()
var var_396 **Struct_std_____path_____FilePath
_ = var_396
var_396 = IDENTITY((self))
var root string
_ = root
root = (*var_396).path_ref
var var_397 Tup_
var_397 = Tup_{}
_ = var_397

            filepath.Walk(root, func(p string, info os.FileInfo, err error) error {
                if err != nil {
                    return nil
                }

                file_paths.elems = append(file_paths.elems, p)
                return nil
            })

var var_398 int
_ = var_398
var_398 = 0
var current_idx int
_ = current_idx
current_idx = var_398
var var_399 func() any
_ = var_399
var_399 = func() any {
var var_400 int
_ = var_400
var_400 = IDENTITY((current_idx))
var var_401 int
_ = var_401
var_401 = file_paths.len()
var var_402 bool
_ = var_402
switch Int_Ord((var_400), &(var_401)).(type) {
                            case Tag__greater:
                            var_402 = true
                            case Tag__equal:
                            var_402 = true
                            default:
                            var_402 = false
                            }

var var_403 Tup_
var_403 = Tup_{}
_ = var_403
if var_402 {
var var_404 Tag__no_next_elem
_ = var_404
var_404 = Tag__no_next_elem{}
return var_404
}
var var_405 int
_ = var_405
var_405 = IDENTITY((current_idx))
var fp *Struct_std_____opt_____Opt_____string
_ = fp
fp = file_paths.get(var_405)
var var_406 bool
_ = var_406
var_406 = fp.is_some()
var var_407 Tup_
var_407 = Tup_{}
_ = var_407
if var_406 {
var var_408 int
_ = var_408
var_408 = IDENTITY((current_idx))
var var_409 int
_ = var_409
var_409 = 1
var var_410 int
_ = var_410
var_410 = var_408 + var_409
current_idx = var_410
var var_411 string
_ = var_411
var_411 = fp.unwrap()
var var_412 Tag__none
_ = var_412
var_412 = Tag__none{}
var var_413 *Struct_std_____path_____FilePath
_ = var_413
var_413 = &Struct_std_____path_____FilePath{path_ref: var_411, last_error: var_412}
return var_413
}
var var_414 Tag__no_next_elem
_ = var_414
var_414 = Tag__no_next_elem{}
return var_414
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_415 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_415
var_415 = std_____col_____Iter_____from_____Struct_std_____path_____FilePath(var_399)
return var_415
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) walk_files() *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_416 bool
_ = var_416
var_416 = (*self).failed()
var var_417 Tup_
var_417 = Tup_{}
_ = var_417
if var_416 {
var var_418 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_418
var_418 = std_____col_____Iter_____empty_____Struct_std_____path_____FilePath()
return var_418
}
var file_paths *Struct_std_____col_____ArrayList_____string
_ = file_paths
file_paths = std_____col_____ArrayList_____new_____string()
var var_419 **Struct_std_____path_____FilePath
_ = var_419
var_419 = IDENTITY((self))
var root string
_ = root
root = (*var_419).path_ref
var var_420 Tup_
var_420 = Tup_{}
_ = var_420

            filepath.Walk(root, func(p string, info os.FileInfo, err error) error {
                if err != nil {
                    return nil
                }

                if !info.IsDir() {
                    file_paths.elems = append(file_paths.elems, p)
                }
                return nil
            })

var var_421 int
_ = var_421
var_421 = 0
var current_idx int
_ = current_idx
current_idx = var_421
var var_422 func() any
_ = var_422
var_422 = func() any {
var var_423 int
_ = var_423
var_423 = IDENTITY((current_idx))
var var_424 int
_ = var_424
var_424 = file_paths.len()
var var_425 bool
_ = var_425
switch Int_Ord((var_423), &(var_424)).(type) {
                            case Tag__greater:
                            var_425 = true
                            case Tag__equal:
                            var_425 = true
                            default:
                            var_425 = false
                            }

var var_426 Tup_
var_426 = Tup_{}
_ = var_426
if var_425 {
var var_427 Tag__no_next_elem
_ = var_427
var_427 = Tag__no_next_elem{}
return var_427
}
var var_428 int
_ = var_428
var_428 = IDENTITY((current_idx))
var fp *Struct_std_____opt_____Opt_____string
_ = fp
fp = file_paths.get(var_428)
var var_429 bool
_ = var_429
var_429 = fp.is_some()
var var_430 Tup_
var_430 = Tup_{}
_ = var_430
if var_429 {
var var_431 int
_ = var_431
var_431 = IDENTITY((current_idx))
var var_432 int
_ = var_432
var_432 = 1
var var_433 int
_ = var_433
var_433 = var_431 + var_432
current_idx = var_433
var var_434 string
_ = var_434
var_434 = fp.unwrap()
var var_435 Tag__none
_ = var_435
var_435 = Tag__none{}
var var_436 *Struct_std_____path_____FilePath
_ = var_436
var_436 = &Struct_std_____path_____FilePath{path_ref: var_434, last_error: var_435}
return var_436
}
var var_437 Tag__no_next_elem
_ = var_437
var_437 = Tag__no_next_elem{}
return var_437
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_438 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_438
var_438 = std_____col_____Iter_____from_____Struct_std_____path_____FilePath(var_422)
return var_438
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) failed() bool {

var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_439 **Struct_std_____path_____FilePath
_ = var_439
var_439 = IDENTITY((self))
var var_440 Tag__none
_ = var_440
var_440 = Tag__none{}
var var_441 bool
_ = var_441
var_441 = func() bool {
                    var p1 any = ((*var_439).last_error)
                    var p2 any = (var_440)

                        switch p1.(type) {
                        case string:
                            switch p2.(type) {
                                case string:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
var var_442 bool
_ = var_442
var_442 = !var_441
return var_442
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) join(part string) *Struct_std_____path_____FilePath {
_ = part
var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_443 string
_ = var_443

            var_443 = filepath.Join((*self).path_ref, part)

var var_444 string
_ = var_444
var_444 = var_443
var new_path string
_ = new_path
new_path = var_444
var var_445 string
_ = var_445
var_445 = IDENTITY((new_path))
var var_446 **Struct_std_____path_____FilePath
_ = var_446
var_446 = IDENTITY((self))
var var_447 *Struct_std_____path_____FilePath
_ = var_447
var_447 = &Struct_std_____path_____FilePath{path_ref: var_445, last_error: (*var_446).last_error}
return var_447
var ΔΔΔretΔΔΔ **Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) parent() *Struct_std_____path_____FilePath {

var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_448 string
_ = var_448

            var_448 = filepath.Dir((*self).path_ref)

var var_449 string
_ = var_449
var_449 = var_448
var parent_path string
_ = parent_path
parent_path = var_449
var var_450 string
_ = var_450
var_450 = IDENTITY((parent_path))
var var_451 **Struct_std_____path_____FilePath
_ = var_451
var_451 = IDENTITY((self))
var var_452 *Struct_std_____path_____FilePath
_ = var_452
var_452 = &Struct_std_____path_____FilePath{path_ref: var_450, last_error: (*var_451).last_error}
return var_452
var ΔΔΔretΔΔΔ **Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) filename() string {

var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_453 string
_ = var_453

            var_453 = filepath.Base((*self).path_ref)

var var_454 string
_ = var_454
var_454 = var_453
return var_454
var ΔΔΔretΔΔΔ *string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) extension() string {

var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_455 string
_ = var_455

            var_455 = filepath.Ext((*self).path_ref)

var var_456 string
_ = var_456
var_456 = var_455
return var_456
var ΔΔΔretΔΔΔ *string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) ensure_dir() **Struct_std_____path_____FilePath {

var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_457 bool
_ = var_457
var_457 = (*self).failed()
var var_458 Tup_
var_458 = Tup_{}
_ = var_458
if var_457 {
var var_459 **Struct_std_____path_____FilePath
_ = var_459
var_459 = IDENTITY((self))
return var_459
}
var var_460 Tup_
var_460 = Tup_{}
_ = var_460

            dir := filepath.Dir((*self).path_ref)
            err := os.MkdirAll(dir, 0755)
            if err != nil {
               	(*self).last_error = err.Error()
            }

var var_461 **Struct_std_____path_____FilePath
_ = var_461
var_461 = IDENTITY((self))
return var_461
var ΔΔΔretΔΔΔ ***Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) write(content string) **Struct_std_____path_____FilePath {
_ = content
var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_462 bool
_ = var_462
var_462 = (*self).failed()
var var_463 Tup_
var_463 = Tup_{}
_ = var_463
if var_462 {
var var_464 **Struct_std_____path_____FilePath
_ = var_464
var_464 = IDENTITY((self))
return var_464
}
var var_466 string
_ = var_466
{
var var_465 **Struct_std_____path_____FilePath
_ = var_465
var_465 = IDENTITY((self))
var_466 = (*var_465).path_ref
}
var var_467 string
_ = var_467
var_467 = var_466 + ".tmp"
var tmp_path string
_ = tmp_path
tmp_path = var_467
var var_468 Tup_
var_468 = Tup_{}
_ = var_468

            err := os.WriteFile(tmp_path, []byte(content), 0644)
            if err != nil {
               	(*self).last_error = err.Error()
               	return self
            }

            err = os.Rename(tmp_path, (*self).path_ref)
            if err != nil {
               	(*self).last_error = err.Error()
               	return self
            }

var var_469 **Struct_std_____path_____FilePath
_ = var_469
var_469 = IDENTITY((self))
return var_469
var ΔΔΔretΔΔΔ ***Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) delete() **Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_470 bool
_ = var_470
var_470 = (*self).failed()
var var_471 Tup_
var_471 = Tup_{}
_ = var_471
if var_470 {
var var_472 **Struct_std_____path_____FilePath
_ = var_472
var_472 = IDENTITY((self))
return var_472
}
var var_473 Tup_
var_473 = Tup_{}
_ = var_473

            err := os.Remove((*self).path_ref)
            if err != nil {
                (*self).last_error = err.Error()
            }

var var_474 **Struct_std_____path_____FilePath
_ = var_474
var_474 = IDENTITY((self))
return var_474
var ΔΔΔretΔΔΔ ***Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) rename(new_path string) **Struct_std_____path_____FilePath {
_ = new_path
var Δorg_addr *Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_475 bool
_ = var_475
var_475 = (*self).failed()
var var_476 Tup_
var_476 = Tup_{}
_ = var_476
if var_475 {
var var_477 **Struct_std_____path_____FilePath
_ = var_477
var_477 = IDENTITY((self))
return var_477
}
var var_478 Tup_
var_478 = Tup_{}
_ = var_478

            if err := os.Rename((*self).path_ref, new_path); err != nil {
               	(*self).last_error = err.Error()
            }

var var_479 string
_ = var_479
var_479 = IDENTITY((new_path))
(*self).path_ref = var_479
var var_480 **Struct_std_____path_____FilePath
_ = var_480
var_480 = IDENTITY((self))
return var_480
var ΔΔΔretΔΔΔ ***Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) read() any {

var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_481 bool
_ = var_481
var_481 = (*self).failed()
var var_482 Tup_
var_482 = Tup_{}
_ = var_482
if var_481 {
var var_483 Tag__err
_ = var_483
var_483 = Tag__err{}
return var_483
}
var var_484 string
_ = var_484

            bs, err := os.ReadFile((*self).path_ref)
            if err != nil {
               	return Tag__err {}
            }

            return string(bs)

var var_485 string
_ = var_485
var_485 = var_484
return var_485
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) size() any {

var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_486 int
_ = var_486

            s, err := os.Stat((*self).path_ref)
            if err != nil {
               	return Tag__err {}
            }

            return int(s.Size())

var var_487 int
_ = var_487
var_487 = int(var_486)
return var_487
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) exists() bool {

var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_488 bool
_ = var_488
var_488 = (*self).failed()
var var_489 Tup_
var_489 = Tup_{}
_ = var_489
if var_488 {
var var_490 bool
_ = var_490
var_490 = false
return var_490
}
var var_491 bool
_ = var_491

            _, err := os.Stat((*self).path_ref)
            var_491 = !os.IsNotExist(err)

var var_492 bool
_ = var_492
var_492 = var_491
return var_492
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____path_____FilePath) expect(msg string) string {
_ = msg
var self **Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_493 **Struct_std_____path_____FilePath
_ = var_493
var_493 = IDENTITY((self))
var var_494 any
_ = var_494

var var_495 any
_ = var_495
var_495 = (*var_493).last_error
if var_497, ok := var_495.(string); ok {
_ = var_497

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_495))
                            _ = iface_ptr

var err string
_ = err
err = IDENTITY((var_497))
if true {
var var_499 string
_ = var_499
{
var var_498 string
_ = var_498
var_498 = IDENTITY((msg))
var_499 = var_498
}
var var_501 string
_ = var_501
{
var var_500 string
_ = var_500
var_500 = IDENTITY((err))
var_501 = var_500
}
var var_502 string
_ = var_502
var_502 = var_499 + ": " + var_501
std_____error_____panic(var_502)
goto var_496

}
}
if var_503, ok := var_495.(Tag__none); ok {
_ = var_503

if true {
var var_504 **Struct_std_____path_____FilePath
_ = var_504
var_504 = IDENTITY((self))
return (*var_504).path_ref
goto var_496

}
}

                    var_496:
                    if false {}

var ΔΔΔretΔΔΔ *string
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____path_____FilePath_Copy(self *Struct_std_____path_____FilePath) *Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____path_____FilePath{
path_ref: IDENTITY((self.path_ref)),
last_error: func() any {
                    var p1 any = (self.last_error)

                        switch p1.(type) {
                        case string:
                            tmp := p1.(string)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }(),
}

}










type Struct_std_____col_____Iter_____string struct {
next_fn func() any
}


type Struct_std_____col_____Iter_____RefMut___string struct {
next_fn func() any
}


type Struct_std_____col_____Iter_____Ref___Ref___string struct {
next_fn func() any
}


type Struct_std_____col_____Iter_____RefMut___Ref___string struct {
next_fn func() any
}


type Struct_std_____col_____ArrayList_____Ref___string struct {
elems []*string
}



func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) push(new_elem *string) **Struct_std_____col_____ArrayList_____Ref___string {
_ = new_elem
var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_520 Tup_
var_520 = Tup_{}
_ = var_520

            (*self).elems = append((*self).elems, new_elem)

var var_521 **Struct_std_____col_____ArrayList_____Ref___string
_ = var_521
var_521 = IDENTITY((self))
return var_521
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) pop() *Struct_std_____opt_____Opt_____Ref___string {

var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_522 int
_ = var_522
var_522 = (*self).len()
var var_523 int
_ = var_523
var_523 = 0
var var_524 bool
_ = var_524
switch Int_Ord((var_522), &(var_523)).(type) {
                            case Tag__greater:
                            var_524 = true
                            default:
                            var_524 = false
                            }

var var_525 Tup_
var_525 = Tup_{}
_ = var_525
if var_524 {
var var_526 *string
_ = var_526

                var index = len((*self).elems) - 1;
                var_526 = (*self).elems[index];
                (*self).elems = (*self).elems[:index];

var var_527 *string
_ = var_527
var_527 = var_526
var value *string
_ = value
value = var_527
var var_528 *string
_ = var_528
var_528 = IDENTITY((value))
var var_529 *Struct_std_____opt_____Opt_____Ref___string
_ = var_529
var_529 = std_____opt_____Opt_____some_____Ref___string(var_528)
return var_529
}
var var_530 *Struct_std_____opt_____Opt_____Ref___string
_ = var_530
var_530 = std_____opt_____Opt_____none_____Ref___string()
return var_530
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) get(index int) *Struct_std_____opt_____Opt_____Ref___string {
_ = index
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
var tag_none *Struct_std_____opt_____Opt_____Ref___string
_ = tag_none
tag_none = std_____opt_____Opt_____none_____Ref___string()
var var_531 *string
_ = var_531

            if index < 0 || index >= (*self).len() {
                return tag_none;
            }

            var_531 = (*self).elems[index]

var var_532 *string
_ = var_532
var_532 = var_531
var elem *string
_ = elem
elem = var_532
var var_533 *string
_ = var_533
var_533 = IDENTITY((elem))
var var_534 *Struct_std_____opt_____Opt_____Ref___string
_ = var_534
var_534 = std_____opt_____Opt_____some_____Ref___string(var_533)
return var_534
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) set(index int, value *string) **Struct_std_____col_____ArrayList_____Ref___string {
_ = index
_ = value
var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_535 Tup_
var_535 = Tup_{}
_ = var_535

            (*self).elems[index] = value

var var_536 **Struct_std_____col_____ArrayList_____Ref___string
_ = var_536
var_536 = IDENTITY((self))
return var_536
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) remove(index int) **Struct_std_____col_____ArrayList_____Ref___string {
_ = index
var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_537 Tup_
var_537 = Tup_{}
_ = var_537

            (*self).elems = append((*self).elems[:index], (*self).elems[index+1:]...)

var var_538 **Struct_std_____col_____ArrayList_____Ref___string
_ = var_538
var_538 = IDENTITY((self))
return var_538
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) len() int {

var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
var var_539 int
_ = var_539
var_539 = 0
var l int
_ = l
l = var_539
var var_540 Tup_
var_540 = Tup_{}
_ = var_540

            l = len((*self).elems);

var var_541 int
_ = var_541
var_541 = IDENTITY((l))
return var_541
var ΔΔΔretΔΔΔ *int
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) is_empty() bool {

var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
var var_542 bool
_ = var_542
var_542 = true
var empty bool
_ = empty
empty = var_542
var var_543 Tup_
var_543 = Tup_{}
_ = var_543

            empty = len((*self).elems) == 0

var var_544 bool
_ = var_544
var_544 = IDENTITY((empty))
return var_544
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) clear() **Struct_std_____col_____ArrayList_____Ref___string {

var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_545 []*string
_ = var_545
var_545 = []*string{}
var var_546 []*string
_ = var_546
var_546 = var_545
(*self).elems = var_546
var var_547 **Struct_std_____col_____ArrayList_____Ref___string
_ = var_547
var_547 = IDENTITY((self))
return var_547
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) filter(f func(x *string) bool) *Struct_std_____col_____ArrayList_____Ref___string {
_ = f
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
var res *Struct_std_____col_____ArrayList_____Ref___string
_ = res
res = std_____col_____ArrayList_____new_____Ref___string()
var var_548 Tup_
var_548 = Tup_{}
_ = var_548

            for _, e := range (*self).elems {
                if f(e) {
                    res.elems = append(res.elems, e)
                }
            }

var var_549 *Struct_std_____col_____ArrayList_____Ref___string
_ = var_549
var_549 = Struct_std_____col_____ArrayList_____Ref___string_Copy((res))
return var_549
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) for_each(f func(e *string) Tup_) Tup_ {
_ = f
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
var var_551 Tup_
var_551 = Tup_{}
_ = var_551
{
var var_550 Tup_
var_550 = Tup_{}
_ = var_550

            for _, e := range (*self).elems {
                f(e)
            }

var_551 = var_550
}
return var_551
return Tup_{}
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) find(f func(x *string) bool) any {
_ = f
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
var var_552 Tag__none
_ = var_552
var_552 = Tag__none{}
var result any
_ = result
result = var_552
var var_553 Tup_
var_553 = Tup_{}
_ = var_553

            for _, e := range (*self).elems {
                if f(e) {
                    result = e
                    break
                }
            }

var var_554 any
_ = var_554
var_554 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case *string:
                            tmp := p1.(*string)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_554
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) contains(something *string) bool {
_ = something
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
var var_555 bool
_ = var_555
var_555 = false
var result bool
_ = result
result = var_555
var var_556 Tup_
var_556 = Tup_{}
_ = var_556

            for _, e := range (*self).elems {
                if e == something {
                    result = true
                    break
                }
            }

var var_557 bool
_ = var_557
var_557 = IDENTITY((result))
return var_557
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) any(f func(x *string) bool) bool {
_ = f
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
var var_558 bool
_ = var_558
var_558 = false
var result bool
_ = result
result = var_558
var var_559 Tup_
var_559 = Tup_{}
_ = var_559

            for _, e := range (*self).elems {
                if f(e) {
                    result = true
                    break
                }
            }

var var_560 bool
_ = var_560
var_560 = IDENTITY((result))
return var_560
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) find_index(predicate func(element *string) bool) any {
_ = predicate
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
var var_561 Tag__none
_ = var_561
var_561 = Tag__none{}
var result any
_ = result
result = var_561
var var_562 Tup_
var_562 = Tup_{}
_ = var_562

            for i, elem := range (*self).elems {
                if predicate(elem) {
                    result = i;
                    break
                }
            }

var var_563 any
_ = var_563
var_563 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case int:
                            tmp := p1.(int)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_563
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) iter() *Struct_std_____col_____Iter_____Ref___Ref___string {

var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
var var_564 int
_ = var_564
var_564 = 0
var idx int
_ = idx
idx = var_564
var var_565 func() any
_ = var_565
var_565 = func() any {
var var_566 int
_ = var_566
var_566 = IDENTITY((idx))
var var_567 int
_ = var_567
var_567 = (*self).len()
var var_568 bool
_ = var_568
switch Int_Ord((var_566), &(var_567)).(type) {
                            case Tag__greater:
                            var_568 = true
                            case Tag__equal:
                            var_568 = true
                            default:
                            var_568 = false
                            }

var var_569 Tup_
var_569 = Tup_{}
_ = var_569
if var_568 {
var var_570 Tag__no_next_elem
_ = var_570
var_570 = Tag__no_next_elem{}
return var_570
}
var var_571 int
_ = var_571
var_571 = IDENTITY((idx))
var var_572 **string
_ = var_572
var_572 = &(*self).elems[var_571]
var elem_to_ret **string
_ = elem_to_ret
elem_to_ret = var_572
var var_573 int
_ = var_573
var_573 = IDENTITY((idx))
var var_574 int
_ = var_574
var_574 = 1
var var_575 int
_ = var_575
var_575 = var_573 + var_574
idx = var_575
var var_576 **string
_ = var_576
var_576 = IDENTITY((elem_to_ret))
return var_576
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_565
var var_577 func() any
_ = var_577
var_577 = IDENTITY((f))
var var_578 *Struct_std_____col_____Iter_____Ref___Ref___string
_ = var_578
var_578 = std_____col_____Iter_____from_____Ref___Ref___string(var_577)
return var_578
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) iter_mut() *Struct_std_____col_____Iter_____RefMut___Ref___string {

var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_579 int
_ = var_579
var_579 = 0
var idx int
_ = idx
idx = var_579
var var_580 func() any
_ = var_580
var_580 = func() any {
var var_581 int
_ = var_581
var_581 = IDENTITY((idx))
var var_582 int
_ = var_582
var_582 = (*self).len()
var var_583 bool
_ = var_583
switch Int_Ord((var_581), &(var_582)).(type) {
                            case Tag__greater:
                            var_583 = true
                            case Tag__equal:
                            var_583 = true
                            default:
                            var_583 = false
                            }

var var_584 Tup_
var_584 = Tup_{}
_ = var_584
if var_583 {
var var_585 Tag__no_next_elem
_ = var_585
var_585 = Tag__no_next_elem{}
return var_585
}
var var_586 int
_ = var_586
var_586 = IDENTITY((idx))
var var_587 **string
_ = var_587
var_587 = &(*self).elems[var_586]
var elem_to_ret **string
_ = elem_to_ret
elem_to_ret = var_587
var var_588 int
_ = var_588
var_588 = IDENTITY((idx))
var var_589 int
_ = var_589
var_589 = 1
var var_590 int
_ = var_590
var_590 = var_588 + var_589
idx = var_590
var var_591 **string
_ = var_591
var_591 = IDENTITY((elem_to_ret))
return var_591
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_580
var var_592 func() any
_ = var_592
var_592 = IDENTITY((f))
var var_593 *Struct_std_____col_____Iter_____RefMut___Ref___string
_ = var_593
var_593 = std_____col_____Iter_____from_____RefMut___Ref___string(var_592)
return var_593
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) as_ref() *[]*string {

var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
var var_594 *[]*string
_ = var_594
var_594 = &(*self).elems
return var_594
var ΔΔΔretΔΔΔ **[]*string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___string) as_mut() *[]*string {

var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_595 *[]*string
_ = var_595
var_595 = &(*self).elems
return var_595
var ΔΔΔretΔΔΔ **[]*string
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____col_____ArrayList_____Ref___string_Copy(self *Struct_std_____col_____ArrayList_____Ref___string) *Struct_std_____col_____ArrayList_____Ref___string {
_ = self
return &Struct_std_____col_____ArrayList_____Ref___string{
elems: Array_Ref___string_Copy((self.elems)),
}

}
type Struct_std_____opt_____Some_____Ref___string struct {
value *string
}

func  Struct_std_____opt_____Some_____Ref___string_Copy(self *Struct_std_____opt_____Some_____Ref___string) *Struct_std_____opt_____Some_____Ref___string {
_ = self
return &Struct_std_____opt_____Some_____Ref___string{
value: IDENTITY((self.value)),
}

}
type Struct_std_____opt_____Opt_____Ref___string struct {
inner any
}



func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___string) is_some() bool {

var self **Struct_std_____opt_____Opt_____Ref___string
_ = self
self = &duck_internal_self
var var_596 **Struct_std_____opt_____Opt_____Ref___string
_ = var_596
var_596 = IDENTITY((self))
var var_597 Tag__none
_ = var_597
var_597 = Tag__none{}
var var_598 bool
_ = var_598
var_598 = func() bool {
                    var p1 any = ((*var_596).inner)
                    var p2 any = (var_597)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Ref___string:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Ref___string:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
var var_599 bool
_ = var_599
var_599 = !var_598
return var_599
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___string) if_some(consumer func(value *string) Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Ref___string
_ = self
self = &duck_internal_self
var var_609 Tup_
var_609 = Tup_{}
_ = var_609
{
var var_600 **Struct_std_____opt_____Opt_____Ref___string
_ = var_600
var_600 = IDENTITY((self))
var var_601 Tup_
var_601 = Tup_{}
_ = var_601

var var_602 any
_ = var_602
var_602 = (*var_600).inner
if var_604, ok := var_602.(Tag__none); ok {
_ = var_604

if true {
var var_605 Tup_
var_605 = Tup_{}
_ = var_605
var_605 = Tup_{}
return var_605
goto var_603

}
}
if var_606, ok := var_602.(*Struct_std_____opt_____Some_____Ref___string); ok {
_ = var_606

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_602))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Ref___string_Copy((var_606))
if true {
var var_607 *Struct_std_____opt_____Some_____Ref___string
_ = var_607
var_607 = Struct_std_____opt_____Some_____Ref___string_Copy((some))
var var_608 Tup_
var_608 = Tup_{}
_ = var_608
var_608 = consumer(var_607.value)
var_601 = var_608
goto var_603

}
}

                    var_603:
                    if false {}

var_609 = var_601
}
return var_609
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___string) if_none(consumer func() Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Ref___string
_ = self
self = &duck_internal_self
var var_618 Tup_
var_618 = Tup_{}
_ = var_618
{
var var_610 **Struct_std_____opt_____Opt_____Ref___string
_ = var_610
var_610 = IDENTITY((self))
var var_611 Tup_
var_611 = Tup_{}
_ = var_611

var var_612 any
_ = var_612
var_612 = (*var_610).inner
if var_614, ok := var_612.(Tag__none); ok {
_ = var_614

if true {
var var_615 Tup_
var_615 = Tup_{}
_ = var_615
var_615 = consumer()
var_611 = var_615
goto var_613

}
}
if var_616, ok := var_612.(*Struct_std_____opt_____Some_____Ref___string); ok {
_ = var_616

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_612))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Ref___string_Copy((var_616))
if true {
var var_617 Tup_
var_617 = Tup_{}
_ = var_617
var_617 = Tup_{}
return var_617
goto var_613

}
}

                    var_613:
                    if false {}

var_618 = var_611
}
return var_618
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___string) is_none() bool {

var self **Struct_std_____opt_____Opt_____Ref___string
_ = self
self = &duck_internal_self
var var_619 **Struct_std_____opt_____Opt_____Ref___string
_ = var_619
var_619 = IDENTITY((self))
var var_620 Tag__none
_ = var_620
var_620 = Tag__none{}
var var_621 bool
_ = var_621
var_621 = func() bool {
                    var p1 any = ((*var_619).inner)
                    var p2 any = (var_620)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Ref___string:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Ref___string:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
return var_621
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___string) filter(predicate func(value *string) bool) *Struct_std_____opt_____Opt_____Ref___string {
_ = predicate
var self **Struct_std_____opt_____Opt_____Ref___string
_ = self
self = &duck_internal_self
var var_622 **Struct_std_____opt_____Opt_____Ref___string
_ = var_622
var_622 = IDENTITY((self))
var var_623 *Struct_std_____opt_____Opt_____Ref___string
_ = var_623

var var_624 any
_ = var_624
var_624 = (*var_622).inner
if var_626, ok := var_624.(Tag__none); ok {
_ = var_626

if true {
var var_627 Tag__none
_ = var_627
var_627 = Tag__none{}
var var_628 *Struct_std_____opt_____Opt_____Ref___string
_ = var_628
var_628 = &Struct_std_____opt_____Opt_____Ref___string{inner: var_627}
var_623 = var_628
goto var_625

}
}
if var_629, ok := var_624.(*Struct_std_____opt_____Some_____Ref___string); ok {
_ = var_629

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_624))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Ref___string_Copy((var_629))
if true {
var var_639 *Struct_std_____opt_____Opt_____Ref___string
_ = var_639
{
var var_630 *Struct_std_____opt_____Some_____Ref___string
_ = var_630
var_630 = Struct_std_____opt_____Some_____Ref___string_Copy((some))
var var_631 bool
_ = var_631
var_631 = predicate(var_630.value)
var var_632 *Struct_std_____opt_____Opt_____Ref___string
_ = var_632
if var_631 {
var var_635 *Struct_std_____opt_____Opt_____Ref___string
_ = var_635
{
var var_633 **Struct_std_____opt_____Opt_____Ref___string
_ = var_633
var_633 = IDENTITY((self))
var var_634 *Struct_std_____opt_____Opt_____Ref___string
_ = var_634
var_634 = *var_633
var_635 = var_634
}
var_632 = var_635
} else {
var var_638 *Struct_std_____opt_____Opt_____Ref___string
_ = var_638
{
var var_636 Tag__none
_ = var_636
var_636 = Tag__none{}
var var_637 *Struct_std_____opt_____Opt_____Ref___string
_ = var_637
var_637 = &Struct_std_____opt_____Opt_____Ref___string{inner: var_636}
var_638 = var_637
}
var_632 = var_638
}
var_639 = var_632
}
var_623 = var_639
goto var_625

}
}

                    var_625:
                    if false {}

return var_623
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___string) or_else(other *string) *string {
_ = other
var self **Struct_std_____opt_____Opt_____Ref___string
_ = self
self = &duck_internal_self
var var_640 **Struct_std_____opt_____Opt_____Ref___string
_ = var_640
var_640 = IDENTITY((self))
var var_641 *string
_ = var_641

var var_642 any
_ = var_642
var_642 = (*var_640).inner
if var_644, ok := var_642.(Tag__none); ok {
_ = var_644

if true {
var var_645 *string
_ = var_645
var_645 = IDENTITY((other))
var_641 = var_645
goto var_643

}
}
if var_646, ok := var_642.(*Struct_std_____opt_____Some_____Ref___string); ok {
_ = var_646

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_642))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Ref___string_Copy((var_646))
if true {
var var_647 *Struct_std_____opt_____Some_____Ref___string
_ = var_647
var_647 = Struct_std_____opt_____Some_____Ref___string_Copy((some))
var_641 = var_647.value
goto var_643

}
}

                    var_643:
                    if false {}

return var_641
var ΔΔΔretΔΔΔ **string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___string) or_else_get(other_supplier func() *string) *string {
_ = other_supplier
var self **Struct_std_____opt_____Opt_____Ref___string
_ = self
self = &duck_internal_self
var var_648 **Struct_std_____opt_____Opt_____Ref___string
_ = var_648
var_648 = IDENTITY((self))
var var_649 *string
_ = var_649

var var_650 any
_ = var_650
var_650 = (*var_648).inner
if var_652, ok := var_650.(Tag__none); ok {
_ = var_652

if true {
var var_653 *string
_ = var_653
var_653 = other_supplier()
var_649 = var_653
goto var_651

}
}
if var_654, ok := var_650.(*Struct_std_____opt_____Some_____Ref___string); ok {
_ = var_654

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_650))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Ref___string_Copy((var_654))
if true {
var var_655 *Struct_std_____opt_____Some_____Ref___string
_ = var_655
var_655 = Struct_std_____opt_____Some_____Ref___string_Copy((some))
var_649 = var_655.value
goto var_651

}
}

                    var_651:
                    if false {}

return var_649
var ΔΔΔretΔΔΔ **string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___string) unwrap() *string {

var self **Struct_std_____opt_____Opt_____Ref___string
_ = self
self = &duck_internal_self
var var_656 **Struct_std_____opt_____Opt_____Ref___string
_ = var_656
var_656 = IDENTITY((self))
var var_657 *string
_ = var_657

var var_658 any
_ = var_658
var_658 = (*var_656).inner
if var_660, ok := var_658.(Tag__none); ok {
_ = var_660

if true {
var var_661 string
_ = var_661
var_661 = "unwrap on none"
std_____error_____panic(var_661)
goto var_659

}
}
if var_662, ok := var_658.(*Struct_std_____opt_____Some_____Ref___string); ok {
_ = var_662

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_658))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Ref___string_Copy((var_662))
if true {
var var_663 *Struct_std_____opt_____Some_____Ref___string
_ = var_663
var_663 = Struct_std_____opt_____Some_____Ref___string_Copy((some))
var_657 = var_663.value
goto var_659

}
}

                    var_659:
                    if false {}

return var_657
var ΔΔΔretΔΔΔ **string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___string) expect(msg string) *string {
_ = msg
var self **Struct_std_____opt_____Opt_____Ref___string
_ = self
self = &duck_internal_self
var var_664 **Struct_std_____opt_____Opt_____Ref___string
_ = var_664
var_664 = IDENTITY((self))
var var_665 *string
_ = var_665

var var_666 any
_ = var_666
var_666 = (*var_664).inner
if var_668, ok := var_666.(Tag__none); ok {
_ = var_668

if true {
var var_670 string
_ = var_670
{
var var_669 string
_ = var_669
var_669 = IDENTITY((msg))
var_670 = var_669
}
var var_671 string
_ = var_671
var_671 = "expect failed: " + var_670
std_____error_____panic(var_671)
goto var_667

}
}
if var_672, ok := var_666.(*Struct_std_____opt_____Some_____Ref___string); ok {
_ = var_672

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_666))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Ref___string_Copy((var_672))
if true {
var var_673 *Struct_std_____opt_____Some_____Ref___string
_ = var_673
var_673 = Struct_std_____opt_____Some_____Ref___string_Copy((some))
var_665 = var_673.value
goto var_667

}
}

                    var_667:
                    if false {}

return var_665
var ΔΔΔretΔΔΔ **string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___string) iter() *Struct_std_____col_____Iter_____Ref___string {

var self **Struct_std_____opt_____Opt_____Ref___string
_ = self
self = &duck_internal_self
var var_689 *Struct_std_____col_____Iter_____Ref___string
_ = var_689
{
var var_674 bool
_ = var_674
var_674 = false
var consumed bool
_ = consumed
consumed = var_674
var var_675 **Struct_std_____opt_____Opt_____Ref___string
_ = var_675
var_675 = IDENTITY((self))
var var_676 *Struct_std_____col_____Iter_____Ref___string
_ = var_676

var var_677 any
_ = var_677
var_677 = (*var_675).inner
if var_679, ok := var_677.(Tag__none); ok {
_ = var_679

if true {
var var_680 *Struct_std_____col_____Iter_____Ref___string
_ = var_680
var_680 = std_____col_____Iter_____empty_____Ref___string()
var_676 = var_680
goto var_678

}
}
if var_681, ok := var_677.(*Struct_std_____opt_____Some_____Ref___string); ok {
_ = var_681

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_677))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Ref___string_Copy((var_681))
if true {
var var_682 func() any
_ = var_682
var_682 = func() any {
var var_683 bool
_ = var_683
var_683 = IDENTITY((consumed))
var var_684 any
_ = var_684
if var_683 {
var var_685 Tag__no_next_elem
_ = var_685
var_685 = Tag__no_next_elem{}
return var_685
} else {
var var_686 bool
_ = var_686
var_686 = true
consumed = var_686
var var_687 *Struct_std_____opt_____Some_____Ref___string
_ = var_687
var_687 = Struct_std_____opt_____Some_____Ref___string_Copy((some))
return var_687.value
}
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_688 *Struct_std_____col_____Iter_____Ref___string
_ = var_688
var_688 = std_____col_____Iter_____from_____Ref___string(var_682)
var_676 = var_688
goto var_678

}
}

                    var_678:
                    if false {}

var_689 = var_676
}
return var_689
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

type Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___string struct {
next_fn func() any
}


type Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___string struct {
next_fn func() any
}


type Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string struct {
elems []*Struct_std_____col_____Iter_____Ref___string
}



func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) push(new_elem *Struct_std_____col_____Iter_____Ref___string) **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string {
_ = new_elem
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_690 Tup_
var_690 = Tup_{}
_ = var_690

            (*self).elems = append((*self).elems, new_elem)

var var_691 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = var_691
var_691 = IDENTITY((self))
return var_691
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) pop() *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_692 int
_ = var_692
var_692 = (*self).len()
var var_693 int
_ = var_693
var_693 = 0
var var_694 bool
_ = var_694
switch Int_Ord((var_692), &(var_693)).(type) {
                            case Tag__greater:
                            var_694 = true
                            default:
                            var_694 = false
                            }

var var_695 Tup_
var_695 = Tup_{}
_ = var_695
if var_694 {
var var_696 *Struct_std_____col_____Iter_____Ref___string
_ = var_696

                var index = len((*self).elems) - 1;
                var_696 = (*self).elems[index];
                (*self).elems = (*self).elems[:index];

var var_697 *Struct_std_____col_____Iter_____Ref___string
_ = var_697
var_697 = var_696
var value *Struct_std_____col_____Iter_____Ref___string
_ = value
value = var_697
var var_698 *Struct_std_____col_____Iter_____Ref___string
_ = var_698
var_698 = Struct_std_____col_____Iter_____Ref___string_Copy((value))
var var_699 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_699
var_699 = std_____opt_____Opt_____some_____Struct_std_____col_____Iter_____Ref___string(var_698)
return var_699
}
var var_700 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_700
var_700 = std_____opt_____Opt_____none_____Struct_std_____col_____Iter_____Ref___string()
return var_700
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) get(index int) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string {
_ = index
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var tag_none *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = tag_none
tag_none = std_____opt_____Opt_____none_____Struct_std_____col_____Iter_____Ref___string()
var var_701 *Struct_std_____col_____Iter_____Ref___string
_ = var_701

            if index < 0 || index >= (*self).len() {
                return tag_none;
            }

            var_701 = (*self).elems[index]

var var_702 *Struct_std_____col_____Iter_____Ref___string
_ = var_702
var_702 = var_701
var elem *Struct_std_____col_____Iter_____Ref___string
_ = elem
elem = var_702
var var_703 *Struct_std_____col_____Iter_____Ref___string
_ = var_703
var_703 = Struct_std_____col_____Iter_____Ref___string_Copy((elem))
var var_704 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_704
var_704 = std_____opt_____Opt_____some_____Struct_std_____col_____Iter_____Ref___string(var_703)
return var_704
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) set(index int, value *Struct_std_____col_____Iter_____Ref___string) **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string {
_ = index
_ = value
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_705 Tup_
var_705 = Tup_{}
_ = var_705

            (*self).elems[index] = value

var var_706 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = var_706
var_706 = IDENTITY((self))
return var_706
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) remove(index int) **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string {
_ = index
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_707 Tup_
var_707 = Tup_{}
_ = var_707

            (*self).elems = append((*self).elems[:index], (*self).elems[index+1:]...)

var var_708 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = var_708
var_708 = IDENTITY((self))
return var_708
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) len() int {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_709 int
_ = var_709
var_709 = 0
var l int
_ = l
l = var_709
var var_710 Tup_
var_710 = Tup_{}
_ = var_710

            l = len((*self).elems);

var var_711 int
_ = var_711
var_711 = IDENTITY((l))
return var_711
var ΔΔΔretΔΔΔ *int
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) is_empty() bool {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_712 bool
_ = var_712
var_712 = true
var empty bool
_ = empty
empty = var_712
var var_713 Tup_
var_713 = Tup_{}
_ = var_713

            empty = len((*self).elems) == 0

var var_714 bool
_ = var_714
var_714 = IDENTITY((empty))
return var_714
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) clear() **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_715 []*Struct_std_____col_____Iter_____Ref___string
_ = var_715
var_715 = []*Struct_std_____col_____Iter_____Ref___string{}
var var_716 []*Struct_std_____col_____Iter_____Ref___string
_ = var_716
var_716 = var_715
(*self).elems = var_716
var var_717 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = var_717
var_717 = IDENTITY((self))
return var_717
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) filter(f func(x *Struct_std_____col_____Iter_____Ref___string) bool) *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var res *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = res
res = std_____col_____ArrayList_____new_____Struct_std_____col_____Iter_____Ref___string()
var var_718 Tup_
var_718 = Tup_{}
_ = var_718

            for _, e := range (*self).elems {
                if f(e) {
                    res.elems = append(res.elems, e)
                }
            }

var var_719 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = var_719
var_719 = Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string_Copy((res))
return var_719
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) for_each(f func(e *Struct_std_____col_____Iter_____Ref___string) Tup_) Tup_ {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_721 Tup_
var_721 = Tup_{}
_ = var_721
{
var var_720 Tup_
var_720 = Tup_{}
_ = var_720

            for _, e := range (*self).elems {
                f(e)
            }

var_721 = var_720
}
return var_721
return Tup_{}
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) find(f func(x *Struct_std_____col_____Iter_____Ref___string) bool) any {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_722 Tag__none
_ = var_722
var_722 = Tag__none{}
var result any
_ = result
result = var_722
var var_723 Tup_
var_723 = Tup_{}
_ = var_723

            for _, e := range (*self).elems {
                if f(e) {
                    result = e
                    break
                }
            }

var var_724 any
_ = var_724
var_724 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case *Struct_std_____col_____Iter_____Ref___string:
                            tmp := p1.(*Struct_std_____col_____Iter_____Ref___string)
                            _ = tmp
                            return Struct_std_____col_____Iter_____Ref___string_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_724
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) contains(something *Struct_std_____col_____Iter_____Ref___string) bool {
_ = something
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_725 bool
_ = var_725
var_725 = false
var result bool
_ = result
result = var_725
var var_726 Tup_
var_726 = Tup_{}
_ = var_726

            for _, e := range (*self).elems {
                if e == something {
                    result = true
                    break
                }
            }

var var_727 bool
_ = var_727
var_727 = IDENTITY((result))
return var_727
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) any(f func(x *Struct_std_____col_____Iter_____Ref___string) bool) bool {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_728 bool
_ = var_728
var_728 = false
var result bool
_ = result
result = var_728
var var_729 Tup_
var_729 = Tup_{}
_ = var_729

            for _, e := range (*self).elems {
                if f(e) {
                    result = true
                    break
                }
            }

var var_730 bool
_ = var_730
var_730 = IDENTITY((result))
return var_730
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) find_index(predicate func(element *Struct_std_____col_____Iter_____Ref___string) bool) any {
_ = predicate
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_731 Tag__none
_ = var_731
var_731 = Tag__none{}
var result any
_ = result
result = var_731
var var_732 Tup_
var_732 = Tup_{}
_ = var_732

            for i, elem := range (*self).elems {
                if predicate(elem) {
                    result = i;
                    break
                }
            }

var var_733 any
_ = var_733
var_733 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case int:
                            tmp := p1.(int)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_733
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) iter() *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___string {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_734 int
_ = var_734
var_734 = 0
var idx int
_ = idx
idx = var_734
var var_735 func() any
_ = var_735
var_735 = func() any {
var var_736 int
_ = var_736
var_736 = IDENTITY((idx))
var var_737 int
_ = var_737
var_737 = (*self).len()
var var_738 bool
_ = var_738
switch Int_Ord((var_736), &(var_737)).(type) {
                            case Tag__greater:
                            var_738 = true
                            case Tag__equal:
                            var_738 = true
                            default:
                            var_738 = false
                            }

var var_739 Tup_
var_739 = Tup_{}
_ = var_739
if var_738 {
var var_740 Tag__no_next_elem
_ = var_740
var_740 = Tag__no_next_elem{}
return var_740
}
var var_741 int
_ = var_741
var_741 = IDENTITY((idx))
var var_742 **Struct_std_____col_____Iter_____Ref___string
_ = var_742
var_742 = &(*self).elems[var_741]
var elem_to_ret **Struct_std_____col_____Iter_____Ref___string
_ = elem_to_ret
elem_to_ret = var_742
var var_743 int
_ = var_743
var_743 = IDENTITY((idx))
var var_744 int
_ = var_744
var_744 = 1
var var_745 int
_ = var_745
var_745 = var_743 + var_744
idx = var_745
var var_746 **Struct_std_____col_____Iter_____Ref___string
_ = var_746
var_746 = IDENTITY((elem_to_ret))
return var_746
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_735
var var_747 func() any
_ = var_747
var_747 = IDENTITY((f))
var var_748 *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___string
_ = var_748
var_748 = std_____col_____Iter_____from_____Ref___Struct_std_____col_____Iter_____Ref___string(var_747)
return var_748
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) iter_mut() *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___string {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_749 int
_ = var_749
var_749 = 0
var idx int
_ = idx
idx = var_749
var var_750 func() any
_ = var_750
var_750 = func() any {
var var_751 int
_ = var_751
var_751 = IDENTITY((idx))
var var_752 int
_ = var_752
var_752 = (*self).len()
var var_753 bool
_ = var_753
switch Int_Ord((var_751), &(var_752)).(type) {
                            case Tag__greater:
                            var_753 = true
                            case Tag__equal:
                            var_753 = true
                            default:
                            var_753 = false
                            }

var var_754 Tup_
var_754 = Tup_{}
_ = var_754
if var_753 {
var var_755 Tag__no_next_elem
_ = var_755
var_755 = Tag__no_next_elem{}
return var_755
}
var var_756 int
_ = var_756
var_756 = IDENTITY((idx))
var var_757 **Struct_std_____col_____Iter_____Ref___string
_ = var_757
var_757 = &(*self).elems[var_756]
var elem_to_ret **Struct_std_____col_____Iter_____Ref___string
_ = elem_to_ret
elem_to_ret = var_757
var var_758 int
_ = var_758
var_758 = IDENTITY((idx))
var var_759 int
_ = var_759
var_759 = 1
var var_760 int
_ = var_760
var_760 = var_758 + var_759
idx = var_760
var var_761 **Struct_std_____col_____Iter_____Ref___string
_ = var_761
var_761 = IDENTITY((elem_to_ret))
return var_761
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_750
var var_762 func() any
_ = var_762
var_762 = IDENTITY((f))
var var_763 *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___string
_ = var_763
var_763 = std_____col_____Iter_____from_____RefMut___Struct_std_____col_____Iter_____Ref___string(var_762)
return var_763
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) as_ref() *[]*Struct_std_____col_____Iter_____Ref___string {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_764 *[]*Struct_std_____col_____Iter_____Ref___string
_ = var_764
var_764 = &(*self).elems
return var_764
var ΔΔΔretΔΔΔ **[]*Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) as_mut() *[]*Struct_std_____col_____Iter_____Ref___string {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_765 *[]*Struct_std_____col_____Iter_____Ref___string
_ = var_765
var_765 = &(*self).elems
return var_765
var ΔΔΔretΔΔΔ **[]*Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string_Copy(self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string) *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string {
_ = self
return &Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string{
elems: Array_Struct_std_____col_____Iter_____Ref___string_Copy((self.elems)),
}

}
type Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string struct {
value *Struct_std_____col_____Iter_____Ref___string
}

func  Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy(self *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string) *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string {
_ = self
return &Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string{
value: Struct_std_____col_____Iter_____Ref___string_Copy((self.value)),
}

}
type Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string struct {
next_fn func() any
}


type Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string struct {
inner any
}



func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string) is_some() bool {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_766 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_766
var_766 = IDENTITY((self))
var var_767 Tag__none
_ = var_767
var_767 = Tag__none{}
var var_768 bool
_ = var_768
var_768 = func() bool {
                    var p1 any = ((*var_766).inner)
                    var p2 any = (var_767)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
var var_769 bool
_ = var_769
var_769 = !var_768
return var_769
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string) if_some(consumer func(value *Struct_std_____col_____Iter_____Ref___string) Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_779 Tup_
var_779 = Tup_{}
_ = var_779
{
var var_770 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_770
var_770 = IDENTITY((self))
var var_771 Tup_
var_771 = Tup_{}
_ = var_771

var var_772 any
_ = var_772
var_772 = (*var_770).inner
if var_774, ok := var_772.(Tag__none); ok {
_ = var_774

if true {
var var_775 Tup_
var_775 = Tup_{}
_ = var_775
var_775 = Tup_{}
return var_775
goto var_773

}
}
if var_776, ok := var_772.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string); ok {
_ = var_776

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_772))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((var_776))
if true {
var var_777 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = var_777
var_777 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((some))
var var_778 Tup_
var_778 = Tup_{}
_ = var_778
var_778 = consumer(var_777.value)
var_771 = var_778
goto var_773

}
}

                    var_773:
                    if false {}

var_779 = var_771
}
return var_779
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string) if_none(consumer func() Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_788 Tup_
var_788 = Tup_{}
_ = var_788
{
var var_780 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_780
var_780 = IDENTITY((self))
var var_781 Tup_
var_781 = Tup_{}
_ = var_781

var var_782 any
_ = var_782
var_782 = (*var_780).inner
if var_784, ok := var_782.(Tag__none); ok {
_ = var_784

if true {
var var_785 Tup_
var_785 = Tup_{}
_ = var_785
var_785 = consumer()
var_781 = var_785
goto var_783

}
}
if var_786, ok := var_782.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string); ok {
_ = var_786

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_782))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((var_786))
if true {
var var_787 Tup_
var_787 = Tup_{}
_ = var_787
var_787 = Tup_{}
return var_787
goto var_783

}
}

                    var_783:
                    if false {}

var_788 = var_781
}
return var_788
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string) is_none() bool {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_789 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_789
var_789 = IDENTITY((self))
var var_790 Tag__none
_ = var_790
var_790 = Tag__none{}
var var_791 bool
_ = var_791
var_791 = func() bool {
                    var p1 any = ((*var_789).inner)
                    var p2 any = (var_790)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
return var_791
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string) filter(predicate func(value *Struct_std_____col_____Iter_____Ref___string) bool) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string {
_ = predicate
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_792 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_792
var_792 = IDENTITY((self))
var var_793 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_793

var var_794 any
_ = var_794
var_794 = (*var_792).inner
if var_796, ok := var_794.(Tag__none); ok {
_ = var_796

if true {
var var_797 Tag__none
_ = var_797
var_797 = Tag__none{}
var var_798 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_798
var_798 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string{inner: var_797}
var_793 = var_798
goto var_795

}
}
if var_799, ok := var_794.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string); ok {
_ = var_799

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_794))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((var_799))
if true {
var var_809 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_809
{
var var_800 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = var_800
var_800 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((some))
var var_801 bool
_ = var_801
var_801 = predicate(var_800.value)
var var_802 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_802
if var_801 {
var var_805 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_805
{
var var_803 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_803
var_803 = IDENTITY((self))
var var_804 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_804
var_804 = *var_803
var_805 = var_804
}
var_802 = var_805
} else {
var var_808 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_808
{
var var_806 Tag__none
_ = var_806
var_806 = Tag__none{}
var var_807 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_807
var_807 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string{inner: var_806}
var_808 = var_807
}
var_802 = var_808
}
var_809 = var_802
}
var_793 = var_809
goto var_795

}
}

                    var_795:
                    if false {}

return var_793
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string) or_else(other *Struct_std_____col_____Iter_____Ref___string) *Struct_std_____col_____Iter_____Ref___string {
_ = other
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_810 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_810
var_810 = IDENTITY((self))
var var_811 *Struct_std_____col_____Iter_____Ref___string
_ = var_811

var var_812 any
_ = var_812
var_812 = (*var_810).inner
if var_814, ok := var_812.(Tag__none); ok {
_ = var_814

if true {
var var_815 *Struct_std_____col_____Iter_____Ref___string
_ = var_815
var_815 = Struct_std_____col_____Iter_____Ref___string_Copy((other))
var_811 = var_815
goto var_813

}
}
if var_816, ok := var_812.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string); ok {
_ = var_816

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_812))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((var_816))
if true {
var var_817 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = var_817
var_817 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((some))
var_811 = var_817.value
goto var_813

}
}

                    var_813:
                    if false {}

return var_811
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string) or_else_get(other_supplier func() *Struct_std_____col_____Iter_____Ref___string) *Struct_std_____col_____Iter_____Ref___string {
_ = other_supplier
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_818 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_818
var_818 = IDENTITY((self))
var var_819 *Struct_std_____col_____Iter_____Ref___string
_ = var_819

var var_820 any
_ = var_820
var_820 = (*var_818).inner
if var_822, ok := var_820.(Tag__none); ok {
_ = var_822

if true {
var var_823 *Struct_std_____col_____Iter_____Ref___string
_ = var_823
var_823 = other_supplier()
var_819 = var_823
goto var_821

}
}
if var_824, ok := var_820.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string); ok {
_ = var_824

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_820))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((var_824))
if true {
var var_825 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = var_825
var_825 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((some))
var_819 = var_825.value
goto var_821

}
}

                    var_821:
                    if false {}

return var_819
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string) unwrap() *Struct_std_____col_____Iter_____Ref___string {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_826 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_826
var_826 = IDENTITY((self))
var var_827 *Struct_std_____col_____Iter_____Ref___string
_ = var_827

var var_828 any
_ = var_828
var_828 = (*var_826).inner
if var_830, ok := var_828.(Tag__none); ok {
_ = var_830

if true {
var var_831 string
_ = var_831
var_831 = "unwrap on none"
std_____error_____panic(var_831)
goto var_829

}
}
if var_832, ok := var_828.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string); ok {
_ = var_832

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_828))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((var_832))
if true {
var var_833 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = var_833
var_833 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((some))
var_827 = var_833.value
goto var_829

}
}

                    var_829:
                    if false {}

return var_827
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string) expect(msg string) *Struct_std_____col_____Iter_____Ref___string {
_ = msg
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_834 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_834
var_834 = IDENTITY((self))
var var_835 *Struct_std_____col_____Iter_____Ref___string
_ = var_835

var var_836 any
_ = var_836
var_836 = (*var_834).inner
if var_838, ok := var_836.(Tag__none); ok {
_ = var_838

if true {
var var_840 string
_ = var_840
{
var var_839 string
_ = var_839
var_839 = IDENTITY((msg))
var_840 = var_839
}
var var_841 string
_ = var_841
var_841 = "expect failed: " + var_840
std_____error_____panic(var_841)
goto var_837

}
}
if var_842, ok := var_836.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string); ok {
_ = var_842

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_836))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((var_842))
if true {
var var_843 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = var_843
var_843 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((some))
var_835 = var_843.value
goto var_837

}
}

                    var_837:
                    if false {}

return var_835
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string) iter() *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
var var_859 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string
_ = var_859
{
var var_844 bool
_ = var_844
var_844 = false
var consumed bool
_ = consumed
consumed = var_844
var var_845 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_845
var_845 = IDENTITY((self))
var var_846 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string
_ = var_846

var var_847 any
_ = var_847
var_847 = (*var_845).inner
if var_849, ok := var_847.(Tag__none); ok {
_ = var_849

if true {
var var_850 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string
_ = var_850
var_850 = std_____col_____Iter_____empty_____Struct_std_____col_____Iter_____Ref___string()
var_846 = var_850
goto var_848

}
}
if var_851, ok := var_847.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string); ok {
_ = var_851

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_847))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((var_851))
if true {
var var_852 func() any
_ = var_852
var_852 = func() any {
var var_853 bool
_ = var_853
var_853 = IDENTITY((consumed))
var var_854 any
_ = var_854
if var_853 {
var var_855 Tag__no_next_elem
_ = var_855
var_855 = Tag__no_next_elem{}
return var_855
} else {
var var_856 bool
_ = var_856
var_856 = true
consumed = var_856
var var_857 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = var_857
var_857 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((some))
return var_857.value
}
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_858 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string
_ = var_858
var_858 = std_____col_____Iter_____from_____Struct_std_____col_____Iter_____Ref___string(var_852)
var_846 = var_858
goto var_848

}
}

                    var_848:
                    if false {}

var_859 = var_846
}
return var_859
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string_Copy(self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string {
_ = self
return &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string{
inner: func() any {
                    var p1 any = (self.inner)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string:
                            tmp := p1.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string)
                            _ = tmp
                            return Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }(),
}

}
type Struct_std_____col_____Iter_____Tup_int_Ref___string struct {
next_fn func() any
}


type Struct_std_____col_____Iter_____Ref___string struct {
next_fn func() any
}



func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) rev() *Struct_std_____col_____Iter_____Ref___string {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_877 *Struct_std_____col_____Iter_____Ref___string
_ = var_877
{
var stack *Struct_std_____col_____ArrayList_____Ref___string
_ = stack
stack = std_____col_____ArrayList_____new_____Ref___string()
var var_860 func() any
_ = var_860
var_860 = func() any {
if false {goto label_862}
label_862:
for {
var var_861 bool
_ = var_861
var_861 = true
if var_861 {
var var_871 **Struct_std_____col_____ArrayList_____Ref___string
_ = var_871
{
var var_863 any
_ = var_863
var_863 = (*self).next_fn()
var var_864 **Struct_std_____col_____ArrayList_____Ref___string
_ = var_864

var var_865 any
_ = var_865
var_865 = var_863
if var_867, ok := var_865.(*string); ok {
_ = var_867

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_865))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_867))
if true {
var var_868 *string
_ = var_868
var_868 = IDENTITY((t))
var var_869 **Struct_std_____col_____ArrayList_____Ref___string
_ = var_869
var_869 = stack.push(var_868)
var_864 = var_869
goto var_866

}
}
if var_870, ok := var_865.(Tag__no_next_elem); ok {
_ = var_870

if true {
break label_862
goto var_866

}
}

                    var_866:
                    if false {}

var_871 = var_864
}
} else {
break label_862
}
}
var maybe_next *Struct_std_____opt_____Opt_____Ref___string
_ = maybe_next
maybe_next = stack.pop()
var var_872 bool
_ = var_872
var_872 = maybe_next.is_some()
var var_873 any
_ = var_873
if var_872 {
var var_874 *string
_ = var_874
var_874 = maybe_next.unwrap()
return var_874
} else {
var var_875 Tag__no_next_elem
_ = var_875
var_875 = Tag__no_next_elem{}
return var_875
}
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_876 *Struct_std_____col_____Iter_____Ref___string
_ = var_876
var_876 = std_____col_____Iter_____from_____Ref___string(var_860)
var_877 = var_876
}
return var_877
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) next() any {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_878 any
_ = var_878
var_878 = (*self).next_fn()
var var_879 any
_ = var_879

var var_880 any
_ = var_880
var_880 = var_878
if var_882, ok := var_880.(*string); ok {
_ = var_882

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_880))
                            _ = iface_ptr

var x *string
_ = x
x = IDENTITY((var_882))
if true {
var var_883 *string
_ = var_883
var_883 = IDENTITY((x))
var_879 = var_883
goto var_881

}
}
if var_884, ok := var_880.(Tag__no_next_elem); ok {
_ = var_884

if true {
var var_885 Tag__no_next_elem
_ = var_885
var_885 = Tag__no_next_elem{}
var_879 = var_885
goto var_881

}
}

                    var_881:
                    if false {}

return var_879
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) for_each(f func(*string) Tup_) Tup_ {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_898 Tup_
var_898 = Tup_{}
_ = var_898
{
if false {goto label_887}
label_887:
for {
var var_886 bool
_ = var_886
var_886 = true
if var_886 {
var var_897 Tup_
var_897 = Tup_{}
_ = var_897
{
var next any
_ = next
next = (*self).next_fn()
var var_888 any
_ = var_888
var_888 = func() any {
                    var p1 any = (next)

                        switch p1.(type) {
                        case *string:
                            tmp := p1.(*string)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_889 Tup_
var_889 = Tup_{}
_ = var_889

var var_890 any
_ = var_890
var_890 = var_888
if var_892, ok := var_890.(*string); ok {
_ = var_892

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_890))
                            _ = iface_ptr

var str *string
_ = str
str = IDENTITY((var_892))
if true {
var var_893 *string
_ = var_893
var_893 = IDENTITY((str))
var var_894 Tup_
var_894 = Tup_{}
_ = var_894
var_894 = f(var_893)
var_889 = var_894
goto var_891

}
}
if var_895, ok := var_890.(Tag__no_next_elem); ok {
_ = var_895

if true {
var var_896 Tup_
var_896 = Tup_{}
_ = var_896
var_896 = Tup_{}
return var_896
goto var_891

}
}

                    var_891:
                    if false {}

var_897 = var_889
}
} else {
break label_887
}
}
var_898 = Tup_{}
}
return var_898
return Tup_{}
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) chain(other_iters any) *Struct_std_____col_____Iter_____Ref___string {
_ = other_iters
var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_899 any
_ = var_899
var_899 = func() any {
                    var p1 any = (other_iters)

                        switch p1.(type) {
                        case []*Struct_std_____col_____Iter_____Ref___string:
                            tmp := p1.([]*Struct_std_____col_____Iter_____Ref___string)
                            _ = tmp
                            return Array_Struct_std_____col_____Iter_____Ref___string_Copy((tmp))
                        }

                        switch p1.(type) {
                        case *Struct_std_____col_____Iter_____Ref___string:
                            tmp := p1.(*Struct_std_____col_____Iter_____Ref___string)
                            _ = tmp
                            return Struct_std_____col_____Iter_____Ref___string_Copy((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_900 []*Struct_std_____col_____Iter_____Ref___string
_ = var_900

var var_901 any
_ = var_901
var_901 = var_899
if var_903, ok := var_901.([]*Struct_std_____col_____Iter_____Ref___string); ok {
_ = var_903

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_901))
                            _ = iface_ptr

var iters []*Struct_std_____col_____Iter_____Ref___string
_ = iters
iters = Array_Struct_std_____col_____Iter_____Ref___string_Copy((var_903))
if true {
var var_904 []*Struct_std_____col_____Iter_____Ref___string
_ = var_904
var_904 = Array_Struct_std_____col_____Iter_____Ref___string_Copy((iters))
var_900 = var_904
goto var_902

}
}
if var_905, ok := var_901.(*Struct_std_____col_____Iter_____Ref___string); ok {
_ = var_905

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_901))
                            _ = iface_ptr

var iter *Struct_std_____col_____Iter_____Ref___string
_ = iter
iter = Struct_std_____col_____Iter_____Ref___string_Copy((var_905))
if true {
var var_906 *Struct_std_____col_____Iter_____Ref___string
_ = var_906
var_906 = Struct_std_____col_____Iter_____Ref___string_Copy((iter))
var var_907 []*Struct_std_____col_____Iter_____Ref___string
_ = var_907
var_907 = []*Struct_std_____col_____Iter_____Ref___string{var_906}
var_900 = var_907
goto var_902

}
}

                    var_902:
                    if false {}

var remaining_iters *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___string
_ = remaining_iters
remaining_iters = std_____col_____ArrayList_____from_array_____Struct_std_____col_____Iter_____Ref___string(var_900)
var var_908 **Struct_std_____col_____Iter_____Ref___string
_ = var_908
var_908 = IDENTITY((self))
var current_iter **Struct_std_____col_____Iter_____Ref___string
_ = current_iter
current_iter = var_908
var var_909 func() any
_ = var_909
var_909 = func() any {
if false {goto label_911}
label_911:
for {
var var_910 bool
_ = var_910
var_910 = true
if var_910 {
var next_elem any
_ = next_elem
next_elem = (*current_iter).next_fn()
var var_912 any
_ = var_912
var_912 = func() any {
                    var p1 any = (next_elem)

                        switch p1.(type) {
                        case *string:
                            tmp := p1.(*string)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_913 Tup_
var_913 = Tup_{}
_ = var_913

var var_914 any
_ = var_914
var_914 = var_912
if var_916, ok := var_914.(*string); ok {
_ = var_916

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_914))
                            _ = iface_ptr

var value *string
_ = value
value = IDENTITY((var_916))
if true {
var var_917 *string
_ = var_917
var_917 = IDENTITY((value))
return var_917
goto var_915

}
}
if var_918, ok := var_914.(Tag__no_next_elem); ok {
_ = var_918

if true {
var var_919 Tup_
var_919 = Tup_{}
_ = var_919
{
var_919 = Tup_{}
}
var_913 = var_919
goto var_915

}
}

                    var_915:
                    if false {}

var var_920 int
_ = var_920
var_920 = remaining_iters.len()
var var_921 int
_ = var_921
var_921 = 0
var var_922 bool
_ = var_922
switch Int_Ord((var_920), &(var_921)).(type) {
                            case Tag__greater:
                            var_922 = true
                            default:
                            var_922 = false
                            }

var var_923 Tup_
var_923 = Tup_{}
_ = var_923
if var_922 {
var next_iter *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = next_iter
next_iter = remaining_iters.pop()
var var_924 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string
_ = var_924
var_924 = Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___string_Copy((next_iter))
var var_925 any
_ = var_925

var var_926 any
_ = var_926
var_926 = var_924.inner
if var_928, ok := var_926.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string); ok {
_ = var_928

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_926))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___string_Copy((var_928))
if true {
var var_929 **Struct_std_____col_____Iter_____Ref___string
_ = var_929
var_929 = &some.value
current_iter = var_929
continue label_911
goto var_927

}
}
if var_930, ok := var_926.(Tag__none); ok {
_ = var_930

if true {
var var_931 Tag__no_next_elem
_ = var_931
var_931 = Tag__no_next_elem{}
return var_931
goto var_927

}
}

                    var_927:
                    if false {}

}
var var_932 Tag__no_next_elem
_ = var_932
var_932 = Tag__no_next_elem{}
return var_932
} else {
break label_911
}
}
var var_933 Tag__no_next_elem
_ = var_933
var_933 = Tag__no_next_elem{}
return var_933
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_909
var var_934 func() any
_ = var_934
var_934 = IDENTITY((next_fn))
var var_935 *Struct_std_____col_____Iter_____Ref___string
_ = var_935
var_935 = std_____col_____Iter_____from_____Ref___string(var_934)
return var_935
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) skip_while(predicate func(*string) bool) *Struct_std_____col_____Iter_____Ref___string {
_ = predicate
var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_953 *Struct_std_____col_____Iter_____Ref___string
_ = var_953
{
var var_936 func() any
_ = var_936
var_936 = func() any {
if false {goto label_938}
label_938:
for {
var var_937 bool
_ = var_937
var_937 = true
if var_937 {
var var_939 any
_ = var_939
var_939 = (*self).next_fn()
var var_940 any
_ = var_940

var var_941 any
_ = var_941
var_941 = var_939
if var_943, ok := var_941.(*string); ok {
_ = var_943

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_941))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_943))
if true {
var var_944 *string
_ = var_944
var_944 = IDENTITY((t))
var var_945 bool
_ = var_945
var_945 = predicate(var_944)
var var_946 any
_ = var_946
if var_945 {
continue label_938
} else {
var var_947 *string
_ = var_947
var_947 = IDENTITY((t))
return var_947
}
var_940 = var_946
goto var_942

}
}
if var_948, ok := var_941.(Tag__no_next_elem); ok {
_ = var_948

if true {
var var_949 Tag__no_next_elem
_ = var_949
var_949 = Tag__no_next_elem{}
return var_949
goto var_942

}
}

                    var_942:
                    if false {}

} else {
break label_938
}
}
var var_950 string
_ = var_950
var_950 = "skip_while"
std_____error_____unreachable(var_950)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_936
var var_951 func() any
_ = var_951
var_951 = IDENTITY((next_fn))
var var_952 *Struct_std_____col_____Iter_____Ref___string
_ = var_952
var_952 = std_____col_____Iter_____from_____Ref___string(var_951)
var_953 = var_952
}
return var_953
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) take_while(predicate func(*string) bool) *Struct_std_____col_____Iter_____Ref___string {
_ = predicate
var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_972 *Struct_std_____col_____Iter_____Ref___string
_ = var_972
{
var var_954 func() any
_ = var_954
var_954 = func() any {
if false {goto label_956}
label_956:
for {
var var_955 bool
_ = var_955
var_955 = true
if var_955 {
var var_957 any
_ = var_957
var_957 = (*self).next_fn()
var var_958 any
_ = var_958

var var_959 any
_ = var_959
var_959 = var_957
if var_961, ok := var_959.(*string); ok {
_ = var_961

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_959))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_961))
if true {
var var_962 *string
_ = var_962
var_962 = IDENTITY((t))
var var_963 bool
_ = var_963
var_963 = predicate(var_962)
var var_964 any
_ = var_964
if var_963 {
var var_965 *string
_ = var_965
var_965 = IDENTITY((t))
return var_965
} else {
var var_966 Tag__no_next_elem
_ = var_966
var_966 = Tag__no_next_elem{}
return var_966
}
var_958 = var_964
goto var_960

}
}
if var_967, ok := var_959.(Tag__no_next_elem); ok {
_ = var_967

if true {
var var_968 Tag__no_next_elem
_ = var_968
var_968 = Tag__no_next_elem{}
return var_968
goto var_960

}
}

                    var_960:
                    if false {}

} else {
break label_956
}
}
var var_969 string
_ = var_969
var_969 = "take_while"
std_____error_____unreachable(var_969)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_954
var var_970 func() any
_ = var_970
var_970 = IDENTITY((next_fn))
var var_971 *Struct_std_____col_____Iter_____Ref___string
_ = var_971
var_971 = std_____col_____Iter_____from_____Ref___string(var_970)
var_972 = var_971
}
return var_972
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) filter(predicate func(*string) bool) *Struct_std_____col_____Iter_____Ref___string {
_ = predicate
var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_990 *Struct_std_____col_____Iter_____Ref___string
_ = var_990
{
var var_973 func() any
_ = var_973
var_973 = func() any {
if false {goto label_975}
label_975:
for {
var var_974 bool
_ = var_974
var_974 = true
if var_974 {
var var_976 any
_ = var_976
var_976 = (*self).next_fn()
var var_977 any
_ = var_977

var var_978 any
_ = var_978
var_978 = var_976
if var_980, ok := var_978.(*string); ok {
_ = var_980

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_978))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_980))
if true {
var var_981 *string
_ = var_981
var_981 = IDENTITY((t))
var var_982 bool
_ = var_982
var_982 = predicate(var_981)
var var_983 any
_ = var_983
if var_982 {
var var_984 *string
_ = var_984
var_984 = IDENTITY((t))
return var_984
} else {
continue label_975
}
goto var_979

}
}
if var_985, ok := var_978.(Tag__no_next_elem); ok {
_ = var_985

if true {
var var_986 Tag__no_next_elem
_ = var_986
var_986 = Tag__no_next_elem{}
return var_986
goto var_979

}
}

                    var_979:
                    if false {}

} else {
break label_975
}
}
var var_987 string
_ = var_987
var_987 = "filter"
std_____error_____unreachable(var_987)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_973
var var_988 func() any
_ = var_988
var_988 = IDENTITY((next_fn))
var var_989 *Struct_std_____col_____Iter_____Ref___string
_ = var_989
var_989 = std_____col_____Iter_____from_____Ref___string(var_988)
var_990 = var_989
}
return var_990
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) skip(amount int) *Struct_std_____col_____Iter_____Ref___string {
_ = amount
var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1013 *Struct_std_____col_____Iter_____Ref___string
_ = var_1013
{
var var_991 int
_ = var_991
var_991 = IDENTITY((amount))
var to_skip int
_ = to_skip
to_skip = var_991
var var_992 func() any
_ = var_992
var_992 = func() any {
if false {goto label_994}
label_994:
for {
var var_993 bool
_ = var_993
var_993 = true
if var_993 {
var next any
_ = next
next = (*self).next_fn()
var var_995 int
_ = var_995
var_995 = IDENTITY((to_skip))
var var_996 int
_ = var_996
var_996 = 0
var var_997 bool
_ = var_997
switch Int_Ord((var_995), &(var_996)).(type) {
                            case Tag__greater:
                            var_997 = true
                            default:
                            var_997 = false
                            }

var var_998 Tup_
var_998 = Tup_{}
_ = var_998
if var_997 {
var var_999 int
_ = var_999
var_999 = IDENTITY((to_skip))
var var_1000 int
_ = var_1000
var_1000 = 1
var var_1001 int
_ = var_1001
var_1001 = var_999 - var_1000
to_skip = var_1001
continue label_994
}
var var_1002 any
_ = var_1002
var_1002 = func() any {
                    var p1 any = (next)

                        switch p1.(type) {
                        case *string:
                            tmp := p1.(*string)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_1003 *string
_ = var_1003

var var_1004 any
_ = var_1004
var_1004 = var_1002
if var_1006, ok := var_1004.(*string); ok {
_ = var_1006

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1004))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_1006))
if true {
var var_1007 *string
_ = var_1007
var_1007 = IDENTITY((t))
var_1003 = var_1007
goto var_1005

}
}
if var_1008, ok := var_1004.(Tag__no_next_elem); ok {
_ = var_1008

if true {
var var_1009 Tag__no_next_elem
_ = var_1009
var_1009 = Tag__no_next_elem{}
return var_1009
goto var_1005

}
}

                    var_1005:
                    if false {}

return var_1003
} else {
break label_994
}
}
var var_1010 string
_ = var_1010
var_1010 = "skip"
std_____error_____unreachable(var_1010)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_992
var var_1011 func() any
_ = var_1011
var_1011 = IDENTITY((next_fn))
var var_1012 *Struct_std_____col_____Iter_____Ref___string
_ = var_1012
var_1012 = std_____col_____Iter_____from_____Ref___string(var_1011)
var_1013 = var_1012
}
return var_1013
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) take(amount int) *Struct_std_____col_____Iter_____Ref___string {
_ = amount
var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1034 *Struct_std_____col_____Iter_____Ref___string
_ = var_1034
{
var var_1014 int
_ = var_1014
var_1014 = 0
var to_take int
_ = to_take
to_take = var_1014
var var_1015 func() any
_ = var_1015
var_1015 = func() any {
var next any
_ = next
next = (*self).next_fn()
var var_1016 int
_ = var_1016
var_1016 = IDENTITY((to_take))
var var_1017 int
_ = var_1017
var_1017 = IDENTITY((amount))
var var_1018 bool
_ = var_1018
switch Int_Ord((var_1016), &(var_1017)).(type) {
                            case Tag__greater:
                            var_1018 = true
                            case Tag__equal:
                            var_1018 = true
                            default:
                            var_1018 = false
                            }

var var_1019 Tup_
var_1019 = Tup_{}
_ = var_1019
if var_1018 {
var var_1020 Tag__no_next_elem
_ = var_1020
var_1020 = Tag__no_next_elem{}
return var_1020
}
var var_1021 int
_ = var_1021
var_1021 = IDENTITY((to_take))
var var_1022 int
_ = var_1022
var_1022 = 1
var var_1023 int
_ = var_1023
var_1023 = var_1021 + var_1022
to_take = var_1023
var var_1024 any
_ = var_1024
var_1024 = func() any {
                    var p1 any = (next)

                        switch p1.(type) {
                        case *string:
                            tmp := p1.(*string)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_1025 *string
_ = var_1025

var var_1026 any
_ = var_1026
var_1026 = var_1024
if var_1028, ok := var_1026.(*string); ok {
_ = var_1028

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1026))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_1028))
if true {
var var_1029 *string
_ = var_1029
var_1029 = IDENTITY((t))
var_1025 = var_1029
goto var_1027

}
}
if var_1030, ok := var_1026.(Tag__no_next_elem); ok {
_ = var_1030

if true {
var var_1031 Tag__no_next_elem
_ = var_1031
var_1031 = Tag__no_next_elem{}
return var_1031
goto var_1027

}
}

                    var_1027:
                    if false {}

return var_1025
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_1015
var var_1032 func() any
_ = var_1032
var_1032 = IDENTITY((next_fn))
var var_1033 *Struct_std_____col_____Iter_____Ref___string
_ = var_1033
var_1033 = std_____col_____Iter_____from_____Ref___string(var_1032)
var_1034 = var_1033
}
return var_1034
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) indexed() *Struct_std_____col_____Iter_____Tup_int_Ref___string {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1035 int
_ = var_1035
var_1035 = 0
var index int
_ = index
index = var_1035
var var_1036 func() any
_ = var_1036
var_1036 = func() any {
var var_1037 any
_ = var_1037
var_1037 = (*self).next_fn()
var var_1038 *string
_ = var_1038

var var_1039 any
_ = var_1039
var_1039 = var_1037
if var_1041, ok := var_1039.(*string); ok {
_ = var_1041

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1039))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_1041))
if true {
var var_1042 *string
_ = var_1042
var_1042 = IDENTITY((t))
var_1038 = var_1042
goto var_1040

}
}
if var_1043, ok := var_1039.(Tag__no_next_elem); ok {
_ = var_1043

if true {
var var_1044 Tag__no_next_elem
_ = var_1044
var_1044 = Tag__no_next_elem{}
return var_1044
goto var_1040

}
}

                    var_1040:
                    if false {}

var next *string
_ = next
next = var_1038
var var_1045 int
_ = var_1045
var_1045 = IDENTITY((index))
var index_to_return int
_ = index_to_return
index_to_return = var_1045
var var_1046 int
_ = var_1046
var_1046 = IDENTITY((index))
var var_1047 int
_ = var_1047
var_1047 = 1
var var_1048 int
_ = var_1048
var_1048 = var_1046 + var_1047
index = var_1048
var var_1049 int
_ = var_1049
var_1049 = IDENTITY((index_to_return))
var var_1050 *string
_ = var_1050
var_1050 = IDENTITY((next))
var var_1051 Tup_int_Ref___string
_ = var_1051
var_1051 = Tup_int_Ref___string{var_1049, var_1050}
return var_1051
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_1036
var var_1052 func() any
_ = var_1052
var_1052 = IDENTITY((next_fn))
var var_1053 *Struct_std_____col_____Iter_____Tup_int_Ref___string
_ = var_1053
var_1053 = std_____col_____Iter_____from_____Tup_int_Ref___string(var_1052)
return var_1053
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Tup_int_Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) reduce(f func(acc **string,elem *string) Tup_) *Struct_std_____opt_____Opt_____Ref___string {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1054 any
_ = var_1054
var_1054 = (*self).next()
var var_1055 *string
_ = var_1055

var var_1056 any
_ = var_1056
var_1056 = var_1054
if var_1058, ok := var_1056.(*string); ok {
_ = var_1058

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1056))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_1058))
if true {
var var_1059 *string
_ = var_1059
var_1059 = IDENTITY((t))
var_1055 = var_1059
goto var_1057

}
}
if var_1060, ok := var_1056.(Tag__no_next_elem); ok {
_ = var_1060

if true {
var var_1061 *Struct_std_____opt_____Opt_____Ref___string
_ = var_1061
var_1061 = std_____opt_____Opt_____none_____Ref___string()
return var_1061
goto var_1057

}
}

                    var_1057:
                    if false {}

var first *string
_ = first
first = var_1055
if false {goto label_1063}
label_1063:
for {
var var_1062 bool
_ = var_1062
var_1062 = true
if var_1062 {
var var_1075 Tup_
var_1075 = Tup_{}
_ = var_1075
{
var var_1064 any
_ = var_1064
var_1064 = (*self).next()
var var_1065 *string
_ = var_1065

var var_1066 any
_ = var_1066
var_1066 = var_1064
if var_1068, ok := var_1066.(*string); ok {
_ = var_1068

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1066))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_1068))
if true {
var var_1069 *string
_ = var_1069
var_1069 = IDENTITY((t))
var_1065 = var_1069
goto var_1067

}
}
if var_1070, ok := var_1066.(Tag__no_next_elem); ok {
_ = var_1070

if true {
break label_1063
goto var_1067

}
}

                    var_1067:
                    if false {}

var elem *string
_ = elem
elem = var_1065
var var_1071 **string
_ = var_1071
var_1071 = &first
var var_1072 *string
_ = var_1072
var_1072 = IDENTITY((elem))
var var_1073 Tup_
var_1073 = Tup_{}
_ = var_1073
var_1073 = f(var_1071, var_1072)
var var_1074 Tup_
var_1074 = Tup_{}
_ = var_1074
var_1074 = Tup_{}
var_1075 = var_1074
}
} else {
break label_1063
}
}
var var_1076 *string
_ = var_1076
var_1076 = IDENTITY((first))
var var_1077 *Struct_std_____opt_____Opt_____Ref___string
_ = var_1077
var_1077 = std_____opt_____Opt_____some_____Ref___string(var_1076)
return var_1077
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) first() *Struct_std_____opt_____Opt_____Ref___string {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1078 any
_ = var_1078
var_1078 = (*self).next_fn()
var var_1079 *Struct_std_____opt_____Opt_____Ref___string
_ = var_1079

var var_1080 any
_ = var_1080
var_1080 = var_1078
if var_1082, ok := var_1080.(*string); ok {
_ = var_1082

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1080))
                            _ = iface_ptr

var value *string
_ = value
value = IDENTITY((var_1082))
if true {
var var_1083 *string
_ = var_1083
var_1083 = IDENTITY((value))
var var_1084 *Struct_std_____opt_____Opt_____Ref___string
_ = var_1084
var_1084 = std_____opt_____Opt_____some_____Ref___string(var_1083)
var_1079 = var_1084
goto var_1081

}
}
if var_1085, ok := var_1080.(Tag__no_next_elem); ok {
_ = var_1085

if true {
var var_1086 *Struct_std_____opt_____Opt_____Ref___string
_ = var_1086
var_1086 = std_____opt_____Opt_____none_____Ref___string()
var_1079 = var_1086
goto var_1081

}
}

                    var_1081:
                    if false {}

return var_1079
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) into_list() *Struct_std_____col_____ArrayList_____Ref___string {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var arr *Struct_std_____col_____ArrayList_____Ref___string
_ = arr
arr = std_____col_____ArrayList_____new_____Ref___string()
if false {goto label_1088}
label_1088:
for {
var var_1087 bool
_ = var_1087
var_1087 = true
if var_1087 {
var var_1098 **Struct_std_____col_____ArrayList_____Ref___string
_ = var_1098
{
var var_1089 any
_ = var_1089
var_1089 = (*self).next_fn()
var var_1090 **Struct_std_____col_____ArrayList_____Ref___string
_ = var_1090

var var_1091 any
_ = var_1091
var_1091 = var_1089
if var_1093, ok := var_1091.(*string); ok {
_ = var_1093

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1091))
                            _ = iface_ptr

var elem *string
_ = elem
elem = IDENTITY((var_1093))
if true {
var var_1094 *string
_ = var_1094
var_1094 = IDENTITY((elem))
var var_1095 **Struct_std_____col_____ArrayList_____Ref___string
_ = var_1095
var_1095 = arr.push(var_1094)
var_1090 = var_1095
goto var_1092

}
}
if var_1096, ok := var_1091.(Tag__no_next_elem); ok {
_ = var_1096

if true {
var var_1097 *Struct_std_____col_____ArrayList_____Ref___string
_ = var_1097
var_1097 = Struct_std_____col_____ArrayList_____Ref___string_Copy((arr))
return var_1097
goto var_1092

}
}

                    var_1092:
                    if false {}

var_1098 = var_1090
}
} else {
break label_1088
}
}
var var_1099 *Struct_std_____col_____ArrayList_____Ref___string
_ = var_1099
var_1099 = Struct_std_____col_____ArrayList_____Ref___string_Copy((arr))
return var_1099
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) count() uint {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1118 uint
_ = var_1118
{
var var_1100 uint
_ = var_1100
var_1100 = 0
var var_1101 uint
_ = var_1101
var_1101 = uint(var_1100)
var result uint
_ = result
result = var_1101
if false {goto label_1103}
label_1103:
for {
var var_1102 bool
_ = var_1102
var_1102 = true
if var_1102 {
var var_1116 Tup_
var_1116 = Tup_{}
_ = var_1116
{
var var_1104 any
_ = var_1104
var_1104 = (*self).next_fn()
var var_1105 Tup_
var_1105 = Tup_{}
_ = var_1105

var var_1106 any
_ = var_1106
var_1106 = var_1104
if var_1108, ok := var_1106.(*string); ok {
_ = var_1108

if true {
var var_1114 Tup_
var_1114 = Tup_{}
_ = var_1114
{
var var_1109 uint
_ = var_1109
var_1109 = IDENTITY((result))
var var_1110 uint
_ = var_1110
var_1110 = 1
var var_1111 uint
_ = var_1111
var_1111 = uint(var_1110)
var var_1112 uint
_ = var_1112
var_1112 = var_1109 + var_1111
result = var_1112
var var_1113 Tup_
var_1113 = Tup_{}
_ = var_1113
var_1113 = Tup_{}
var_1114 = var_1113
}
var_1105 = var_1114
goto var_1107

}
}
if var_1115, ok := var_1106.(Tag__no_next_elem); ok {
_ = var_1115

if true {
break label_1103
goto var_1107

}
}

                    var_1107:
                    if false {}

var_1116 = var_1105
}
} else {
break label_1103
}
}
var var_1117 uint
_ = var_1117
var_1117 = IDENTITY((result))
var_1118 = var_1117
}
return var_1118
var ΔΔΔretΔΔΔ *uint
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) find(f func(e *string) bool) *Struct_std_____opt_____Opt_____Ref___string {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
if false {goto label_1120}
label_1120:
for {
var var_1119 bool
_ = var_1119
var_1119 = true
if var_1119 {
var var_1133 Tup_
var_1133 = Tup_{}
_ = var_1133
{
var var_1121 any
_ = var_1121
var_1121 = (*self).next_fn()
var var_1122 Tup_
var_1122 = Tup_{}
_ = var_1122

var var_1123 any
_ = var_1123
var_1123 = var_1121
if var_1125, ok := var_1123.(*string); ok {
_ = var_1125

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1123))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_1125))
if true {
var var_1131 Tup_
var_1131 = Tup_{}
_ = var_1131
{
var var_1126 *string
_ = var_1126
var_1126 = IDENTITY((t))
var var_1127 bool
_ = var_1127
var_1127 = f(var_1126)
var var_1128 Tup_
var_1128 = Tup_{}
_ = var_1128
if var_1127 {
var var_1129 *string
_ = var_1129
var_1129 = IDENTITY((t))
var var_1130 *Struct_std_____opt_____Opt_____Ref___string
_ = var_1130
var_1130 = std_____opt_____Opt_____some_____Ref___string(var_1129)
return var_1130
}
var_1131 = var_1128
}
var_1122 = var_1131
goto var_1124

}
}
if var_1132, ok := var_1123.(Tag__no_next_elem); ok {
_ = var_1132

if true {
break label_1120
goto var_1124

}
}

                    var_1124:
                    if false {}

var_1133 = var_1122
}
} else {
break label_1120
}
}
var var_1134 *Struct_std_____opt_____Opt_____Ref___string
_ = var_1134
var_1134 = std_____opt_____Opt_____none_____Ref___string()
return var_1134
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) any(f func(e *string) bool) bool {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1150 bool
_ = var_1150
{
if false {goto label_1136}
label_1136:
for {
var var_1135 bool
_ = var_1135
var_1135 = true
if var_1135 {
var var_1148 Tup_
var_1148 = Tup_{}
_ = var_1148
{
var var_1137 any
_ = var_1137
var_1137 = (*self).next_fn()
var var_1138 Tup_
var_1138 = Tup_{}
_ = var_1138

var var_1139 any
_ = var_1139
var_1139 = var_1137
if var_1141, ok := var_1139.(*string); ok {
_ = var_1141

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1139))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_1141))
if true {
var var_1146 Tup_
var_1146 = Tup_{}
_ = var_1146
{
var var_1142 *string
_ = var_1142
var_1142 = IDENTITY((t))
var var_1143 bool
_ = var_1143
var_1143 = f(var_1142)
var var_1144 Tup_
var_1144 = Tup_{}
_ = var_1144
if var_1143 {
var var_1145 bool
_ = var_1145
var_1145 = true
return var_1145
}
var_1146 = var_1144
}
var_1138 = var_1146
goto var_1140

}
}
if var_1147, ok := var_1139.(Tag__no_next_elem); ok {
_ = var_1147

if true {
break label_1136
goto var_1140

}
}

                    var_1140:
                    if false {}

var_1148 = var_1138
}
} else {
break label_1136
}
}
var var_1149 bool
_ = var_1149
var_1149 = false
var_1150 = var_1149
}
return var_1150
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___string) all(f func(e *string) bool) bool {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Ref___string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1167 bool
_ = var_1167
{
if false {goto label_1152}
label_1152:
for {
var var_1151 bool
_ = var_1151
var_1151 = true
if var_1151 {
var var_1165 Tup_
var_1165 = Tup_{}
_ = var_1165
{
var var_1153 any
_ = var_1153
var_1153 = (*self).next_fn()
var var_1154 Tup_
var_1154 = Tup_{}
_ = var_1154

var var_1155 any
_ = var_1155
var_1155 = var_1153
if var_1157, ok := var_1155.(*string); ok {
_ = var_1157

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1155))
                            _ = iface_ptr

var t *string
_ = t
t = IDENTITY((var_1157))
if true {
var var_1163 Tup_
var_1163 = Tup_{}
_ = var_1163
{
var var_1158 *string
_ = var_1158
var_1158 = IDENTITY((t))
var var_1159 bool
_ = var_1159
var_1159 = f(var_1158)
var var_1160 bool
_ = var_1160
var_1160 = !var_1159
var var_1161 Tup_
var_1161 = Tup_{}
_ = var_1161
if var_1160 {
var var_1162 bool
_ = var_1162
var_1162 = false
return var_1162
}
var_1163 = var_1161
}
var_1154 = var_1163
goto var_1156

}
}
if var_1164, ok := var_1155.(Tag__no_next_elem); ok {
_ = var_1164

if true {
break label_1152
goto var_1156

}
}

                    var_1156:
                    if false {}

var_1165 = var_1154
}
} else {
break label_1152
}
}
var var_1166 bool
_ = var_1166
var_1166 = true
var_1167 = var_1166
}
return var_1167
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____col_____Iter_____Ref___string_Copy(self *Struct_std_____col_____Iter_____Ref___string) *Struct_std_____col_____Iter_____Ref___string {
_ = self
return &Struct_std_____col_____Iter_____Ref___string{
next_fn: IDENTITY((self.next_fn)),
}

}
type Struct_std_____col_____Iter_____RefMut___Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath struct {
elems []*Struct_std_____path_____FilePath
}



func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) push(new_elem *Struct_std_____path_____FilePath) **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath {
_ = new_elem
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1168 Tup_
var_1168 = Tup_{}
_ = var_1168

            (*self).elems = append((*self).elems, new_elem)

var var_1169 **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_1169
var_1169 = IDENTITY((self))
return var_1169
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) pop() *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1170 int
_ = var_1170
var_1170 = (*self).len()
var var_1171 int
_ = var_1171
var_1171 = 0
var var_1172 bool
_ = var_1172
switch Int_Ord((var_1170), &(var_1171)).(type) {
                            case Tag__greater:
                            var_1172 = true
                            default:
                            var_1172 = false
                            }

var var_1173 Tup_
var_1173 = Tup_{}
_ = var_1173
if var_1172 {
var var_1174 *Struct_std_____path_____FilePath
_ = var_1174

                var index = len((*self).elems) - 1;
                var_1174 = (*self).elems[index];
                (*self).elems = (*self).elems[:index];

var var_1175 *Struct_std_____path_____FilePath
_ = var_1175
var_1175 = var_1174
var value *Struct_std_____path_____FilePath
_ = value
value = var_1175
var var_1176 *Struct_std_____path_____FilePath
_ = var_1176
var_1176 = Struct_std_____path_____FilePath_Copy((value))
var var_1177 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_1177
var_1177 = std_____opt_____Opt_____some_____Struct_std_____path_____FilePath(var_1176)
return var_1177
}
var var_1178 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_1178
var_1178 = std_____opt_____Opt_____none_____Struct_std_____path_____FilePath()
return var_1178
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) get(index int) *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath {
_ = index
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var tag_none *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = tag_none
tag_none = std_____opt_____Opt_____none_____Struct_std_____path_____FilePath()
var var_1179 *Struct_std_____path_____FilePath
_ = var_1179

            if index < 0 || index >= (*self).len() {
                return tag_none;
            }

            var_1179 = (*self).elems[index]

var var_1180 *Struct_std_____path_____FilePath
_ = var_1180
var_1180 = var_1179
var elem *Struct_std_____path_____FilePath
_ = elem
elem = var_1180
var var_1181 *Struct_std_____path_____FilePath
_ = var_1181
var_1181 = Struct_std_____path_____FilePath_Copy((elem))
var var_1182 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_1182
var_1182 = std_____opt_____Opt_____some_____Struct_std_____path_____FilePath(var_1181)
return var_1182
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) set(index int, value *Struct_std_____path_____FilePath) **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath {
_ = index
_ = value
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1183 Tup_
var_1183 = Tup_{}
_ = var_1183

            (*self).elems[index] = value

var var_1184 **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_1184
var_1184 = IDENTITY((self))
return var_1184
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) remove(index int) **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath {
_ = index
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1185 Tup_
var_1185 = Tup_{}
_ = var_1185

            (*self).elems = append((*self).elems[:index], (*self).elems[index+1:]...)

var var_1186 **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_1186
var_1186 = IDENTITY((self))
return var_1186
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) len() int {

var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1187 int
_ = var_1187
var_1187 = 0
var l int
_ = l
l = var_1187
var var_1188 Tup_
var_1188 = Tup_{}
_ = var_1188

            l = len((*self).elems);

var var_1189 int
_ = var_1189
var_1189 = IDENTITY((l))
return var_1189
var ΔΔΔretΔΔΔ *int
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) is_empty() bool {

var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1190 bool
_ = var_1190
var_1190 = true
var empty bool
_ = empty
empty = var_1190
var var_1191 Tup_
var_1191 = Tup_{}
_ = var_1191

            empty = len((*self).elems) == 0

var var_1192 bool
_ = var_1192
var_1192 = IDENTITY((empty))
return var_1192
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) clear() **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1193 []*Struct_std_____path_____FilePath
_ = var_1193
var_1193 = []*Struct_std_____path_____FilePath{}
var var_1194 []*Struct_std_____path_____FilePath
_ = var_1194
var_1194 = var_1193
(*self).elems = var_1194
var var_1195 **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_1195
var_1195 = IDENTITY((self))
return var_1195
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) filter(f func(x *Struct_std_____path_____FilePath) bool) *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var res *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = res
res = std_____col_____ArrayList_____new_____Struct_std_____path_____FilePath()
var var_1196 Tup_
var_1196 = Tup_{}
_ = var_1196

            for _, e := range (*self).elems {
                if f(e) {
                    res.elems = append(res.elems, e)
                }
            }

var var_1197 *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_1197
var_1197 = Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath_Copy((res))
return var_1197
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) for_each(f func(e *Struct_std_____path_____FilePath) Tup_) Tup_ {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1199 Tup_
var_1199 = Tup_{}
_ = var_1199
{
var var_1198 Tup_
var_1198 = Tup_{}
_ = var_1198

            for _, e := range (*self).elems {
                f(e)
            }

var_1199 = var_1198
}
return var_1199
return Tup_{}
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) find(f func(x *Struct_std_____path_____FilePath) bool) any {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1200 Tag__none
_ = var_1200
var_1200 = Tag__none{}
var result any
_ = result
result = var_1200
var var_1201 Tup_
var_1201 = Tup_{}
_ = var_1201

            for _, e := range (*self).elems {
                if f(e) {
                    result = e
                    break
                }
            }

var var_1202 any
_ = var_1202
var_1202 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case *Struct_std_____path_____FilePath:
                            tmp := p1.(*Struct_std_____path_____FilePath)
                            _ = tmp
                            return Struct_std_____path_____FilePath_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_1202
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) contains(something *Struct_std_____path_____FilePath) bool {
_ = something
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1203 bool
_ = var_1203
var_1203 = false
var result bool
_ = result
result = var_1203
var var_1204 Tup_
var_1204 = Tup_{}
_ = var_1204

            for _, e := range (*self).elems {
                if e == something {
                    result = true
                    break
                }
            }

var var_1205 bool
_ = var_1205
var_1205 = IDENTITY((result))
return var_1205
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) any(f func(x *Struct_std_____path_____FilePath) bool) bool {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1206 bool
_ = var_1206
var_1206 = false
var result bool
_ = result
result = var_1206
var var_1207 Tup_
var_1207 = Tup_{}
_ = var_1207

            for _, e := range (*self).elems {
                if f(e) {
                    result = true
                    break
                }
            }

var var_1208 bool
_ = var_1208
var_1208 = IDENTITY((result))
return var_1208
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) find_index(predicate func(element *Struct_std_____path_____FilePath) bool) any {
_ = predicate
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1209 Tag__none
_ = var_1209
var_1209 = Tag__none{}
var result any
_ = result
result = var_1209
var var_1210 Tup_
var_1210 = Tup_{}
_ = var_1210

            for i, elem := range (*self).elems {
                if predicate(elem) {
                    result = i;
                    break
                }
            }

var var_1211 any
_ = var_1211
var_1211 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case int:
                            tmp := p1.(int)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_1211
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) iter() *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1212 int
_ = var_1212
var_1212 = 0
var idx int
_ = idx
idx = var_1212
var var_1213 func() any
_ = var_1213
var_1213 = func() any {
var var_1214 int
_ = var_1214
var_1214 = IDENTITY((idx))
var var_1215 int
_ = var_1215
var_1215 = (*self).len()
var var_1216 bool
_ = var_1216
switch Int_Ord((var_1214), &(var_1215)).(type) {
                            case Tag__greater:
                            var_1216 = true
                            case Tag__equal:
                            var_1216 = true
                            default:
                            var_1216 = false
                            }

var var_1217 Tup_
var_1217 = Tup_{}
_ = var_1217
if var_1216 {
var var_1218 Tag__no_next_elem
_ = var_1218
var_1218 = Tag__no_next_elem{}
return var_1218
}
var var_1219 int
_ = var_1219
var_1219 = IDENTITY((idx))
var var_1220 **Struct_std_____path_____FilePath
_ = var_1220
var_1220 = &(*self).elems[var_1219]
var elem_to_ret **Struct_std_____path_____FilePath
_ = elem_to_ret
elem_to_ret = var_1220
var var_1221 int
_ = var_1221
var_1221 = IDENTITY((idx))
var var_1222 int
_ = var_1222
var_1222 = 1
var var_1223 int
_ = var_1223
var_1223 = var_1221 + var_1222
idx = var_1223
var var_1224 **Struct_std_____path_____FilePath
_ = var_1224
var_1224 = IDENTITY((elem_to_ret))
return var_1224
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_1213
var var_1225 func() any
_ = var_1225
var_1225 = IDENTITY((f))
var var_1226 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1226
var_1226 = std_____col_____Iter_____from_____Ref___Struct_std_____path_____FilePath(var_1225)
return var_1226
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) iter_mut() *Struct_std_____col_____Iter_____RefMut___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1227 int
_ = var_1227
var_1227 = 0
var idx int
_ = idx
idx = var_1227
var var_1228 func() any
_ = var_1228
var_1228 = func() any {
var var_1229 int
_ = var_1229
var_1229 = IDENTITY((idx))
var var_1230 int
_ = var_1230
var_1230 = (*self).len()
var var_1231 bool
_ = var_1231
switch Int_Ord((var_1229), &(var_1230)).(type) {
                            case Tag__greater:
                            var_1231 = true
                            case Tag__equal:
                            var_1231 = true
                            default:
                            var_1231 = false
                            }

var var_1232 Tup_
var_1232 = Tup_{}
_ = var_1232
if var_1231 {
var var_1233 Tag__no_next_elem
_ = var_1233
var_1233 = Tag__no_next_elem{}
return var_1233
}
var var_1234 int
_ = var_1234
var_1234 = IDENTITY((idx))
var var_1235 **Struct_std_____path_____FilePath
_ = var_1235
var_1235 = &(*self).elems[var_1234]
var elem_to_ret **Struct_std_____path_____FilePath
_ = elem_to_ret
elem_to_ret = var_1235
var var_1236 int
_ = var_1236
var_1236 = IDENTITY((idx))
var var_1237 int
_ = var_1237
var_1237 = 1
var var_1238 int
_ = var_1238
var_1238 = var_1236 + var_1237
idx = var_1238
var var_1239 **Struct_std_____path_____FilePath
_ = var_1239
var_1239 = IDENTITY((elem_to_ret))
return var_1239
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_1228
var var_1240 func() any
_ = var_1240
var_1240 = IDENTITY((f))
var var_1241 *Struct_std_____col_____Iter_____RefMut___Struct_std_____path_____FilePath
_ = var_1241
var_1241 = std_____col_____Iter_____from_____RefMut___Struct_std_____path_____FilePath(var_1240)
return var_1241
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) as_ref() *[]*Struct_std_____path_____FilePath {

var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1242 *[]*Struct_std_____path_____FilePath
_ = var_1242
var_1242 = &(*self).elems
return var_1242
var ΔΔΔretΔΔΔ **[]*Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) as_mut() *[]*Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1243 *[]*Struct_std_____path_____FilePath
_ = var_1243
var_1243 = &(*self).elems
return var_1243
var ΔΔΔretΔΔΔ **[]*Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath_Copy(self *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath) *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath{
elems: Array_Struct_std_____path_____FilePath_Copy((self.elems)),
}

}
type Struct_std_____col_____Iter_____Ref___Ref___Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____col_____Iter_____RefMut___Ref___Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath struct {
elems []**Struct_std_____path_____FilePath
}



func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) push(new_elem **Struct_std_____path_____FilePath) **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath {
_ = new_elem
var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1244 Tup_
var_1244 = Tup_{}
_ = var_1244

            (*self).elems = append((*self).elems, new_elem)

var var_1245 **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1245
var_1245 = IDENTITY((self))
return var_1245
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) pop() *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1246 int
_ = var_1246
var_1246 = (*self).len()
var var_1247 int
_ = var_1247
var_1247 = 0
var var_1248 bool
_ = var_1248
switch Int_Ord((var_1246), &(var_1247)).(type) {
                            case Tag__greater:
                            var_1248 = true
                            default:
                            var_1248 = false
                            }

var var_1249 Tup_
var_1249 = Tup_{}
_ = var_1249
if var_1248 {
var var_1250 **Struct_std_____path_____FilePath
_ = var_1250

                var index = len((*self).elems) - 1;
                var_1250 = (*self).elems[index];
                (*self).elems = (*self).elems[:index];

var var_1251 **Struct_std_____path_____FilePath
_ = var_1251
var_1251 = var_1250
var value **Struct_std_____path_____FilePath
_ = value
value = var_1251
var var_1252 **Struct_std_____path_____FilePath
_ = var_1252
var_1252 = IDENTITY((value))
var var_1253 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1253
var_1253 = std_____opt_____Opt_____some_____Ref___Struct_std_____path_____FilePath(var_1252)
return var_1253
}
var var_1254 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1254
var_1254 = std_____opt_____Opt_____none_____Ref___Struct_std_____path_____FilePath()
return var_1254
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) get(index int) *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath {
_ = index
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var tag_none *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = tag_none
tag_none = std_____opt_____Opt_____none_____Ref___Struct_std_____path_____FilePath()
var var_1255 **Struct_std_____path_____FilePath
_ = var_1255

            if index < 0 || index >= (*self).len() {
                return tag_none;
            }

            var_1255 = (*self).elems[index]

var var_1256 **Struct_std_____path_____FilePath
_ = var_1256
var_1256 = var_1255
var elem **Struct_std_____path_____FilePath
_ = elem
elem = var_1256
var var_1257 **Struct_std_____path_____FilePath
_ = var_1257
var_1257 = IDENTITY((elem))
var var_1258 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1258
var_1258 = std_____opt_____Opt_____some_____Ref___Struct_std_____path_____FilePath(var_1257)
return var_1258
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) set(index int, value **Struct_std_____path_____FilePath) **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath {
_ = index
_ = value
var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1259 Tup_
var_1259 = Tup_{}
_ = var_1259

            (*self).elems[index] = value

var var_1260 **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1260
var_1260 = IDENTITY((self))
return var_1260
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) remove(index int) **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath {
_ = index
var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1261 Tup_
var_1261 = Tup_{}
_ = var_1261

            (*self).elems = append((*self).elems[:index], (*self).elems[index+1:]...)

var var_1262 **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1262
var_1262 = IDENTITY((self))
return var_1262
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) len() int {

var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1263 int
_ = var_1263
var_1263 = 0
var l int
_ = l
l = var_1263
var var_1264 Tup_
var_1264 = Tup_{}
_ = var_1264

            l = len((*self).elems);

var var_1265 int
_ = var_1265
var_1265 = IDENTITY((l))
return var_1265
var ΔΔΔretΔΔΔ *int
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) is_empty() bool {

var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1266 bool
_ = var_1266
var_1266 = true
var empty bool
_ = empty
empty = var_1266
var var_1267 Tup_
var_1267 = Tup_{}
_ = var_1267

            empty = len((*self).elems) == 0

var var_1268 bool
_ = var_1268
var_1268 = IDENTITY((empty))
return var_1268
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) clear() **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1269 []**Struct_std_____path_____FilePath
_ = var_1269
var_1269 = []**Struct_std_____path_____FilePath{}
var var_1270 []**Struct_std_____path_____FilePath
_ = var_1270
var_1270 = var_1269
(*self).elems = var_1270
var var_1271 **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1271
var_1271 = IDENTITY((self))
return var_1271
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) filter(f func(x **Struct_std_____path_____FilePath) bool) *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath {
_ = f
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var res *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = res
res = std_____col_____ArrayList_____new_____Ref___Struct_std_____path_____FilePath()
var var_1272 Tup_
var_1272 = Tup_{}
_ = var_1272

            for _, e := range (*self).elems {
                if f(e) {
                    res.elems = append(res.elems, e)
                }
            }

var var_1273 *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1273
var_1273 = Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath_Copy((res))
return var_1273
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) for_each(f func(e **Struct_std_____path_____FilePath) Tup_) Tup_ {
_ = f
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1275 Tup_
var_1275 = Tup_{}
_ = var_1275
{
var var_1274 Tup_
var_1274 = Tup_{}
_ = var_1274

            for _, e := range (*self).elems {
                f(e)
            }

var_1275 = var_1274
}
return var_1275
return Tup_{}
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) find(f func(x **Struct_std_____path_____FilePath) bool) any {
_ = f
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1276 Tag__none
_ = var_1276
var_1276 = Tag__none{}
var result any
_ = result
result = var_1276
var var_1277 Tup_
var_1277 = Tup_{}
_ = var_1277

            for _, e := range (*self).elems {
                if f(e) {
                    result = e
                    break
                }
            }

var var_1278 any
_ = var_1278
var_1278 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case **Struct_std_____path_____FilePath:
                            tmp := p1.(**Struct_std_____path_____FilePath)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_1278
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) contains(something **Struct_std_____path_____FilePath) bool {
_ = something
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1279 bool
_ = var_1279
var_1279 = false
var result bool
_ = result
result = var_1279
var var_1280 Tup_
var_1280 = Tup_{}
_ = var_1280

            for _, e := range (*self).elems {
                if e == something {
                    result = true
                    break
                }
            }

var var_1281 bool
_ = var_1281
var_1281 = IDENTITY((result))
return var_1281
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) any(f func(x **Struct_std_____path_____FilePath) bool) bool {
_ = f
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1282 bool
_ = var_1282
var_1282 = false
var result bool
_ = result
result = var_1282
var var_1283 Tup_
var_1283 = Tup_{}
_ = var_1283

            for _, e := range (*self).elems {
                if f(e) {
                    result = true
                    break
                }
            }

var var_1284 bool
_ = var_1284
var_1284 = IDENTITY((result))
return var_1284
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) find_index(predicate func(element **Struct_std_____path_____FilePath) bool) any {
_ = predicate
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1285 Tag__none
_ = var_1285
var_1285 = Tag__none{}
var result any
_ = result
result = var_1285
var var_1286 Tup_
var_1286 = Tup_{}
_ = var_1286

            for i, elem := range (*self).elems {
                if predicate(elem) {
                    result = i;
                    break
                }
            }

var var_1287 any
_ = var_1287
var_1287 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case int:
                            tmp := p1.(int)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_1287
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) iter() *Struct_std_____col_____Iter_____Ref___Ref___Struct_std_____path_____FilePath {

var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1288 int
_ = var_1288
var_1288 = 0
var idx int
_ = idx
idx = var_1288
var var_1289 func() any
_ = var_1289
var_1289 = func() any {
var var_1290 int
_ = var_1290
var_1290 = IDENTITY((idx))
var var_1291 int
_ = var_1291
var_1291 = (*self).len()
var var_1292 bool
_ = var_1292
switch Int_Ord((var_1290), &(var_1291)).(type) {
                            case Tag__greater:
                            var_1292 = true
                            case Tag__equal:
                            var_1292 = true
                            default:
                            var_1292 = false
                            }

var var_1293 Tup_
var_1293 = Tup_{}
_ = var_1293
if var_1292 {
var var_1294 Tag__no_next_elem
_ = var_1294
var_1294 = Tag__no_next_elem{}
return var_1294
}
var var_1295 int
_ = var_1295
var_1295 = IDENTITY((idx))
var var_1296 ***Struct_std_____path_____FilePath
_ = var_1296
var_1296 = &(*self).elems[var_1295]
var elem_to_ret ***Struct_std_____path_____FilePath
_ = elem_to_ret
elem_to_ret = var_1296
var var_1297 int
_ = var_1297
var_1297 = IDENTITY((idx))
var var_1298 int
_ = var_1298
var_1298 = 1
var var_1299 int
_ = var_1299
var_1299 = var_1297 + var_1298
idx = var_1299
var var_1300 ***Struct_std_____path_____FilePath
_ = var_1300
var_1300 = IDENTITY((elem_to_ret))
return var_1300
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_1289
var var_1301 func() any
_ = var_1301
var_1301 = IDENTITY((f))
var var_1302 *Struct_std_____col_____Iter_____Ref___Ref___Struct_std_____path_____FilePath
_ = var_1302
var_1302 = std_____col_____Iter_____from_____Ref___Ref___Struct_std_____path_____FilePath(var_1301)
return var_1302
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) iter_mut() *Struct_std_____col_____Iter_____RefMut___Ref___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1303 int
_ = var_1303
var_1303 = 0
var idx int
_ = idx
idx = var_1303
var var_1304 func() any
_ = var_1304
var_1304 = func() any {
var var_1305 int
_ = var_1305
var_1305 = IDENTITY((idx))
var var_1306 int
_ = var_1306
var_1306 = (*self).len()
var var_1307 bool
_ = var_1307
switch Int_Ord((var_1305), &(var_1306)).(type) {
                            case Tag__greater:
                            var_1307 = true
                            case Tag__equal:
                            var_1307 = true
                            default:
                            var_1307 = false
                            }

var var_1308 Tup_
var_1308 = Tup_{}
_ = var_1308
if var_1307 {
var var_1309 Tag__no_next_elem
_ = var_1309
var_1309 = Tag__no_next_elem{}
return var_1309
}
var var_1310 int
_ = var_1310
var_1310 = IDENTITY((idx))
var var_1311 ***Struct_std_____path_____FilePath
_ = var_1311
var_1311 = &(*self).elems[var_1310]
var elem_to_ret ***Struct_std_____path_____FilePath
_ = elem_to_ret
elem_to_ret = var_1311
var var_1312 int
_ = var_1312
var_1312 = IDENTITY((idx))
var var_1313 int
_ = var_1313
var_1313 = 1
var var_1314 int
_ = var_1314
var_1314 = var_1312 + var_1313
idx = var_1314
var var_1315 ***Struct_std_____path_____FilePath
_ = var_1315
var_1315 = IDENTITY((elem_to_ret))
return var_1315
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_1304
var var_1316 func() any
_ = var_1316
var_1316 = IDENTITY((f))
var var_1317 *Struct_std_____col_____Iter_____RefMut___Ref___Struct_std_____path_____FilePath
_ = var_1317
var_1317 = std_____col_____Iter_____from_____RefMut___Ref___Struct_std_____path_____FilePath(var_1316)
return var_1317
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) as_ref() *[]**Struct_std_____path_____FilePath {

var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1318 *[]**Struct_std_____path_____FilePath
_ = var_1318
var_1318 = &(*self).elems
return var_1318
var ΔΔΔretΔΔΔ **[]**Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) as_mut() *[]**Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1319 *[]**Struct_std_____path_____FilePath
_ = var_1319
var_1319 = &(*self).elems
return var_1319
var ΔΔΔretΔΔΔ **[]**Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath_Copy(self *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath) *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath{
elems: Array_Ref___Struct_std_____path_____FilePath_Copy((self.elems)),
}

}
type Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath struct {
value **Struct_std_____path_____FilePath
}

func  Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy(self *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath) *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath{
value: IDENTITY((self.value)),
}

}
type Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath struct {
inner any
}



func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath) is_some() bool {

var self **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1320 **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1320
var_1320 = IDENTITY((self))
var var_1321 Tag__none
_ = var_1321
var_1321 = Tag__none{}
var var_1322 bool
_ = var_1322
var_1322 = func() bool {
                    var p1 any = ((*var_1320).inner)
                    var p2 any = (var_1321)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
var var_1323 bool
_ = var_1323
var_1323 = !var_1322
return var_1323
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath) if_some(consumer func(value **Struct_std_____path_____FilePath) Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1333 Tup_
var_1333 = Tup_{}
_ = var_1333
{
var var_1324 **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1324
var_1324 = IDENTITY((self))
var var_1325 Tup_
var_1325 = Tup_{}
_ = var_1325

var var_1326 any
_ = var_1326
var_1326 = (*var_1324).inner
if var_1328, ok := var_1326.(Tag__none); ok {
_ = var_1328

if true {
var var_1329 Tup_
var_1329 = Tup_{}
_ = var_1329
var_1329 = Tup_{}
return var_1329
goto var_1327

}
}
if var_1330, ok := var_1326.(*Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1330

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1326))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((var_1330))
if true {
var var_1331 *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = var_1331
var_1331 = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((some))
var var_1332 Tup_
var_1332 = Tup_{}
_ = var_1332
var_1332 = consumer(var_1331.value)
var_1325 = var_1332
goto var_1327

}
}

                    var_1327:
                    if false {}

var_1333 = var_1325
}
return var_1333
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath) if_none(consumer func() Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1342 Tup_
var_1342 = Tup_{}
_ = var_1342
{
var var_1334 **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1334
var_1334 = IDENTITY((self))
var var_1335 Tup_
var_1335 = Tup_{}
_ = var_1335

var var_1336 any
_ = var_1336
var_1336 = (*var_1334).inner
if var_1338, ok := var_1336.(Tag__none); ok {
_ = var_1338

if true {
var var_1339 Tup_
var_1339 = Tup_{}
_ = var_1339
var_1339 = consumer()
var_1335 = var_1339
goto var_1337

}
}
if var_1340, ok := var_1336.(*Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1340

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1336))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((var_1340))
if true {
var var_1341 Tup_
var_1341 = Tup_{}
_ = var_1341
var_1341 = Tup_{}
return var_1341
goto var_1337

}
}

                    var_1337:
                    if false {}

var_1342 = var_1335
}
return var_1342
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath) is_none() bool {

var self **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1343 **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1343
var_1343 = IDENTITY((self))
var var_1344 Tag__none
_ = var_1344
var_1344 = Tag__none{}
var var_1345 bool
_ = var_1345
var_1345 = func() bool {
                    var p1 any = ((*var_1343).inner)
                    var p2 any = (var_1344)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
return var_1345
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath) filter(predicate func(value **Struct_std_____path_____FilePath) bool) *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath {
_ = predicate
var self **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1346 **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1346
var_1346 = IDENTITY((self))
var var_1347 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1347

var var_1348 any
_ = var_1348
var_1348 = (*var_1346).inner
if var_1350, ok := var_1348.(Tag__none); ok {
_ = var_1350

if true {
var var_1351 Tag__none
_ = var_1351
var_1351 = Tag__none{}
var var_1352 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1352
var_1352 = &Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath{inner: var_1351}
var_1347 = var_1352
goto var_1349

}
}
if var_1353, ok := var_1348.(*Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1353

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1348))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((var_1353))
if true {
var var_1363 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1363
{
var var_1354 *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = var_1354
var_1354 = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((some))
var var_1355 bool
_ = var_1355
var_1355 = predicate(var_1354.value)
var var_1356 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1356
if var_1355 {
var var_1359 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1359
{
var var_1357 **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1357
var_1357 = IDENTITY((self))
var var_1358 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1358
var_1358 = *var_1357
var_1359 = var_1358
}
var_1356 = var_1359
} else {
var var_1362 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1362
{
var var_1360 Tag__none
_ = var_1360
var_1360 = Tag__none{}
var var_1361 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1361
var_1361 = &Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath{inner: var_1360}
var_1362 = var_1361
}
var_1356 = var_1362
}
var_1363 = var_1356
}
var_1347 = var_1363
goto var_1349

}
}

                    var_1349:
                    if false {}

return var_1347
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath) or_else(other **Struct_std_____path_____FilePath) **Struct_std_____path_____FilePath {
_ = other
var self **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1364 **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1364
var_1364 = IDENTITY((self))
var var_1365 **Struct_std_____path_____FilePath
_ = var_1365

var var_1366 any
_ = var_1366
var_1366 = (*var_1364).inner
if var_1368, ok := var_1366.(Tag__none); ok {
_ = var_1368

if true {
var var_1369 **Struct_std_____path_____FilePath
_ = var_1369
var_1369 = IDENTITY((other))
var_1365 = var_1369
goto var_1367

}
}
if var_1370, ok := var_1366.(*Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1370

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1366))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((var_1370))
if true {
var var_1371 *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = var_1371
var_1371 = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((some))
var_1365 = var_1371.value
goto var_1367

}
}

                    var_1367:
                    if false {}

return var_1365
var ΔΔΔretΔΔΔ ***Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath) or_else_get(other_supplier func() **Struct_std_____path_____FilePath) **Struct_std_____path_____FilePath {
_ = other_supplier
var self **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1372 **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1372
var_1372 = IDENTITY((self))
var var_1373 **Struct_std_____path_____FilePath
_ = var_1373

var var_1374 any
_ = var_1374
var_1374 = (*var_1372).inner
if var_1376, ok := var_1374.(Tag__none); ok {
_ = var_1376

if true {
var var_1377 **Struct_std_____path_____FilePath
_ = var_1377
var_1377 = other_supplier()
var_1373 = var_1377
goto var_1375

}
}
if var_1378, ok := var_1374.(*Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1378

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1374))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((var_1378))
if true {
var var_1379 *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = var_1379
var_1379 = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((some))
var_1373 = var_1379.value
goto var_1375

}
}

                    var_1375:
                    if false {}

return var_1373
var ΔΔΔretΔΔΔ ***Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath) unwrap() **Struct_std_____path_____FilePath {

var self **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1380 **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1380
var_1380 = IDENTITY((self))
var var_1381 **Struct_std_____path_____FilePath
_ = var_1381

var var_1382 any
_ = var_1382
var_1382 = (*var_1380).inner
if var_1384, ok := var_1382.(Tag__none); ok {
_ = var_1384

if true {
var var_1385 string
_ = var_1385
var_1385 = "unwrap on none"
std_____error_____panic(var_1385)
goto var_1383

}
}
if var_1386, ok := var_1382.(*Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1386

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1382))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((var_1386))
if true {
var var_1387 *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = var_1387
var_1387 = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((some))
var_1381 = var_1387.value
goto var_1383

}
}

                    var_1383:
                    if false {}

return var_1381
var ΔΔΔretΔΔΔ ***Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath) expect(msg string) **Struct_std_____path_____FilePath {
_ = msg
var self **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1388 **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1388
var_1388 = IDENTITY((self))
var var_1389 **Struct_std_____path_____FilePath
_ = var_1389

var var_1390 any
_ = var_1390
var_1390 = (*var_1388).inner
if var_1392, ok := var_1390.(Tag__none); ok {
_ = var_1392

if true {
var var_1394 string
_ = var_1394
{
var var_1393 string
_ = var_1393
var_1393 = IDENTITY((msg))
var_1394 = var_1393
}
var var_1395 string
_ = var_1395
var_1395 = "expect failed: " + var_1394
std_____error_____panic(var_1395)
goto var_1391

}
}
if var_1396, ok := var_1390.(*Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1396

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1390))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((var_1396))
if true {
var var_1397 *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = var_1397
var_1397 = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((some))
var_1389 = var_1397.value
goto var_1391

}
}

                    var_1391:
                    if false {}

return var_1389
var ΔΔΔretΔΔΔ ***Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath) iter() *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var self **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1413 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1413
{
var var_1398 bool
_ = var_1398
var_1398 = false
var consumed bool
_ = consumed
consumed = var_1398
var var_1399 **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1399
var_1399 = IDENTITY((self))
var var_1400 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1400

var var_1401 any
_ = var_1401
var_1401 = (*var_1399).inner
if var_1403, ok := var_1401.(Tag__none); ok {
_ = var_1403

if true {
var var_1404 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1404
var_1404 = std_____col_____Iter_____empty_____Ref___Struct_std_____path_____FilePath()
var_1400 = var_1404
goto var_1402

}
}
if var_1405, ok := var_1401.(*Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1405

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1401))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((var_1405))
if true {
var var_1406 func() any
_ = var_1406
var_1406 = func() any {
var var_1407 bool
_ = var_1407
var_1407 = IDENTITY((consumed))
var var_1408 any
_ = var_1408
if var_1407 {
var var_1409 Tag__no_next_elem
_ = var_1409
var_1409 = Tag__no_next_elem{}
return var_1409
} else {
var var_1410 bool
_ = var_1410
var_1410 = true
consumed = var_1410
var var_1411 *Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath
_ = var_1411
var_1411 = Struct_std_____opt_____Some_____Ref___Struct_std_____path_____FilePath_Copy((some))
return var_1411.value
}
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_1412 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1412
var_1412 = std_____col_____Iter_____from_____Ref___Struct_std_____path_____FilePath(var_1406)
var_1400 = var_1412
goto var_1402

}
}

                    var_1402:
                    if false {}

var_1413 = var_1400
}
return var_1413
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

type Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath struct {
elems []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
}



func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) push(new_elem *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = new_elem
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1414 Tup_
var_1414 = Tup_{}
_ = var_1414

            (*self).elems = append((*self).elems, new_elem)

var var_1415 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1415
var_1415 = IDENTITY((self))
return var_1415
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) pop() *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1416 int
_ = var_1416
var_1416 = (*self).len()
var var_1417 int
_ = var_1417
var_1417 = 0
var var_1418 bool
_ = var_1418
switch Int_Ord((var_1416), &(var_1417)).(type) {
                            case Tag__greater:
                            var_1418 = true
                            default:
                            var_1418 = false
                            }

var var_1419 Tup_
var_1419 = Tup_{}
_ = var_1419
if var_1418 {
var var_1420 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1420

                var index = len((*self).elems) - 1;
                var_1420 = (*self).elems[index];
                (*self).elems = (*self).elems[:index];

var var_1421 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1421
var_1421 = var_1420
var value *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = value
value = var_1421
var var_1422 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1422
var_1422 = Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((value))
var var_1423 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1423
var_1423 = std_____opt_____Opt_____some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath(var_1422)
return var_1423
}
var var_1424 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1424
var_1424 = std_____opt_____Opt_____none_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath()
return var_1424
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) get(index int) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = index
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var tag_none *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = tag_none
tag_none = std_____opt_____Opt_____none_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath()
var var_1425 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1425

            if index < 0 || index >= (*self).len() {
                return tag_none;
            }

            var_1425 = (*self).elems[index]

var var_1426 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1426
var_1426 = var_1425
var elem *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = elem
elem = var_1426
var var_1427 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1427
var_1427 = Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((elem))
var var_1428 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1428
var_1428 = std_____opt_____Opt_____some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath(var_1427)
return var_1428
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) set(index int, value *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = index
_ = value
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1429 Tup_
var_1429 = Tup_{}
_ = var_1429

            (*self).elems[index] = value

var var_1430 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1430
var_1430 = IDENTITY((self))
return var_1430
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) remove(index int) **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = index
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1431 Tup_
var_1431 = Tup_{}
_ = var_1431

            (*self).elems = append((*self).elems[:index], (*self).elems[index+1:]...)

var var_1432 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1432
var_1432 = IDENTITY((self))
return var_1432
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) len() int {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1433 int
_ = var_1433
var_1433 = 0
var l int
_ = l
l = var_1433
var var_1434 Tup_
var_1434 = Tup_{}
_ = var_1434

            l = len((*self).elems);

var var_1435 int
_ = var_1435
var_1435 = IDENTITY((l))
return var_1435
var ΔΔΔretΔΔΔ *int
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) is_empty() bool {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1436 bool
_ = var_1436
var_1436 = true
var empty bool
_ = empty
empty = var_1436
var var_1437 Tup_
var_1437 = Tup_{}
_ = var_1437

            empty = len((*self).elems) == 0

var var_1438 bool
_ = var_1438
var_1438 = IDENTITY((empty))
return var_1438
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) clear() **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1439 []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1439
var_1439 = []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{}
var var_1440 []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1440
var_1440 = var_1439
(*self).elems = var_1440
var var_1441 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1441
var_1441 = IDENTITY((self))
return var_1441
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) filter(f func(x *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) bool) *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var res *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = res
res = std_____col_____ArrayList_____new_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath()
var var_1442 Tup_
var_1442 = Tup_{}
_ = var_1442

            for _, e := range (*self).elems {
                if f(e) {
                    res.elems = append(res.elems, e)
                }
            }

var var_1443 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1443
var_1443 = Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((res))
return var_1443
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) for_each(f func(e *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) Tup_) Tup_ {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1445 Tup_
var_1445 = Tup_{}
_ = var_1445
{
var var_1444 Tup_
var_1444 = Tup_{}
_ = var_1444

            for _, e := range (*self).elems {
                f(e)
            }

var_1445 = var_1444
}
return var_1445
return Tup_{}
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) find(f func(x *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) bool) any {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1446 Tag__none
_ = var_1446
var_1446 = Tag__none{}
var result any
_ = result
result = var_1446
var var_1447 Tup_
var_1447 = Tup_{}
_ = var_1447

            for _, e := range (*self).elems {
                if f(e) {
                    result = e
                    break
                }
            }

var var_1448 any
_ = var_1448
var_1448 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath:
                            tmp := p1.(*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath)
                            _ = tmp
                            return Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_1448
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) contains(something *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) bool {
_ = something
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1449 bool
_ = var_1449
var_1449 = false
var result bool
_ = result
result = var_1449
var var_1450 Tup_
var_1450 = Tup_{}
_ = var_1450

            for _, e := range (*self).elems {
                if e == something {
                    result = true
                    break
                }
            }

var var_1451 bool
_ = var_1451
var_1451 = IDENTITY((result))
return var_1451
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) any(f func(x *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) bool) bool {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1452 bool
_ = var_1452
var_1452 = false
var result bool
_ = result
result = var_1452
var var_1453 Tup_
var_1453 = Tup_{}
_ = var_1453

            for _, e := range (*self).elems {
                if f(e) {
                    result = true
                    break
                }
            }

var var_1454 bool
_ = var_1454
var_1454 = IDENTITY((result))
return var_1454
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) find_index(predicate func(element *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) bool) any {
_ = predicate
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1455 Tag__none
_ = var_1455
var_1455 = Tag__none{}
var result any
_ = result
result = var_1455
var var_1456 Tup_
var_1456 = Tup_{}
_ = var_1456

            for i, elem := range (*self).elems {
                if predicate(elem) {
                    result = i;
                    break
                }
            }

var var_1457 any
_ = var_1457
var_1457 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case int:
                            tmp := p1.(int)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_1457
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) iter() *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1458 int
_ = var_1458
var_1458 = 0
var idx int
_ = idx
idx = var_1458
var var_1459 func() any
_ = var_1459
var_1459 = func() any {
var var_1460 int
_ = var_1460
var_1460 = IDENTITY((idx))
var var_1461 int
_ = var_1461
var_1461 = (*self).len()
var var_1462 bool
_ = var_1462
switch Int_Ord((var_1460), &(var_1461)).(type) {
                            case Tag__greater:
                            var_1462 = true
                            case Tag__equal:
                            var_1462 = true
                            default:
                            var_1462 = false
                            }

var var_1463 Tup_
var_1463 = Tup_{}
_ = var_1463
if var_1462 {
var var_1464 Tag__no_next_elem
_ = var_1464
var_1464 = Tag__no_next_elem{}
return var_1464
}
var var_1465 int
_ = var_1465
var_1465 = IDENTITY((idx))
var var_1466 **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1466
var_1466 = &(*self).elems[var_1465]
var elem_to_ret **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = elem_to_ret
elem_to_ret = var_1466
var var_1467 int
_ = var_1467
var_1467 = IDENTITY((idx))
var var_1468 int
_ = var_1468
var_1468 = 1
var var_1469 int
_ = var_1469
var_1469 = var_1467 + var_1468
idx = var_1469
var var_1470 **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1470
var_1470 = IDENTITY((elem_to_ret))
return var_1470
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_1459
var var_1471 func() any
_ = var_1471
var_1471 = IDENTITY((f))
var var_1472 *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1472
var_1472 = std_____col_____Iter_____from_____Ref___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath(var_1471)
return var_1472
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) iter_mut() *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1473 int
_ = var_1473
var_1473 = 0
var idx int
_ = idx
idx = var_1473
var var_1474 func() any
_ = var_1474
var_1474 = func() any {
var var_1475 int
_ = var_1475
var_1475 = IDENTITY((idx))
var var_1476 int
_ = var_1476
var_1476 = (*self).len()
var var_1477 bool
_ = var_1477
switch Int_Ord((var_1475), &(var_1476)).(type) {
                            case Tag__greater:
                            var_1477 = true
                            case Tag__equal:
                            var_1477 = true
                            default:
                            var_1477 = false
                            }

var var_1478 Tup_
var_1478 = Tup_{}
_ = var_1478
if var_1477 {
var var_1479 Tag__no_next_elem
_ = var_1479
var_1479 = Tag__no_next_elem{}
return var_1479
}
var var_1480 int
_ = var_1480
var_1480 = IDENTITY((idx))
var var_1481 **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1481
var_1481 = &(*self).elems[var_1480]
var elem_to_ret **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = elem_to_ret
elem_to_ret = var_1481
var var_1482 int
_ = var_1482
var_1482 = IDENTITY((idx))
var var_1483 int
_ = var_1483
var_1483 = 1
var var_1484 int
_ = var_1484
var_1484 = var_1482 + var_1483
idx = var_1484
var var_1485 **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1485
var_1485 = IDENTITY((elem_to_ret))
return var_1485
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_1474
var var_1486 func() any
_ = var_1486
var_1486 = IDENTITY((f))
var var_1487 *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1487
var_1487 = std_____col_____Iter_____from_____RefMut___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath(var_1486)
return var_1487
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) as_ref() *[]*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1488 *[]*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1488
var_1488 = &(*self).elems
return var_1488
var ΔΔΔretΔΔΔ **[]*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) as_mut() *[]*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1489 *[]*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1489
var_1489 = &(*self).elems
return var_1489
var ΔΔΔretΔΔΔ **[]*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy(self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{
elems: Array_Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((self.elems)),
}

}
type Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath struct {
value *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
}

func  Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy(self *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{
value: Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((self.value)),
}

}
type Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath struct {
inner any
}



func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) is_some() bool {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1490 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1490
var_1490 = IDENTITY((self))
var var_1491 Tag__none
_ = var_1491
var_1491 = Tag__none{}
var var_1492 bool
_ = var_1492
var_1492 = func() bool {
                    var p1 any = ((*var_1490).inner)
                    var p2 any = (var_1491)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
var var_1493 bool
_ = var_1493
var_1493 = !var_1492
return var_1493
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) if_some(consumer func(value *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1503 Tup_
var_1503 = Tup_{}
_ = var_1503
{
var var_1494 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1494
var_1494 = IDENTITY((self))
var var_1495 Tup_
var_1495 = Tup_{}
_ = var_1495

var var_1496 any
_ = var_1496
var_1496 = (*var_1494).inner
if var_1498, ok := var_1496.(Tag__none); ok {
_ = var_1498

if true {
var var_1499 Tup_
var_1499 = Tup_{}
_ = var_1499
var_1499 = Tup_{}
return var_1499
goto var_1497

}
}
if var_1500, ok := var_1496.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1500

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1496))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((var_1500))
if true {
var var_1501 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1501
var_1501 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((some))
var var_1502 Tup_
var_1502 = Tup_{}
_ = var_1502
var_1502 = consumer(var_1501.value)
var_1495 = var_1502
goto var_1497

}
}

                    var_1497:
                    if false {}

var_1503 = var_1495
}
return var_1503
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) if_none(consumer func() Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1512 Tup_
var_1512 = Tup_{}
_ = var_1512
{
var var_1504 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1504
var_1504 = IDENTITY((self))
var var_1505 Tup_
var_1505 = Tup_{}
_ = var_1505

var var_1506 any
_ = var_1506
var_1506 = (*var_1504).inner
if var_1508, ok := var_1506.(Tag__none); ok {
_ = var_1508

if true {
var var_1509 Tup_
var_1509 = Tup_{}
_ = var_1509
var_1509 = consumer()
var_1505 = var_1509
goto var_1507

}
}
if var_1510, ok := var_1506.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1510

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1506))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((var_1510))
if true {
var var_1511 Tup_
var_1511 = Tup_{}
_ = var_1511
var_1511 = Tup_{}
return var_1511
goto var_1507

}
}

                    var_1507:
                    if false {}

var_1512 = var_1505
}
return var_1512
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) is_none() bool {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1513 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1513
var_1513 = IDENTITY((self))
var var_1514 Tag__none
_ = var_1514
var_1514 = Tag__none{}
var var_1515 bool
_ = var_1515
var_1515 = func() bool {
                    var p1 any = ((*var_1513).inner)
                    var p2 any = (var_1514)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
return var_1515
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) filter(predicate func(value *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) bool) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = predicate
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1516 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1516
var_1516 = IDENTITY((self))
var var_1517 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1517

var var_1518 any
_ = var_1518
var_1518 = (*var_1516).inner
if var_1520, ok := var_1518.(Tag__none); ok {
_ = var_1520

if true {
var var_1521 Tag__none
_ = var_1521
var_1521 = Tag__none{}
var var_1522 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1522
var_1522 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{inner: var_1521}
var_1517 = var_1522
goto var_1519

}
}
if var_1523, ok := var_1518.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1523

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1518))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((var_1523))
if true {
var var_1533 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1533
{
var var_1524 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1524
var_1524 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((some))
var var_1525 bool
_ = var_1525
var_1525 = predicate(var_1524.value)
var var_1526 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1526
if var_1525 {
var var_1529 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1529
{
var var_1527 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1527
var_1527 = IDENTITY((self))
var var_1528 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1528
var_1528 = *var_1527
var_1529 = var_1528
}
var_1526 = var_1529
} else {
var var_1532 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1532
{
var var_1530 Tag__none
_ = var_1530
var_1530 = Tag__none{}
var var_1531 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1531
var_1531 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{inner: var_1530}
var_1532 = var_1531
}
var_1526 = var_1532
}
var_1533 = var_1526
}
var_1517 = var_1533
goto var_1519

}
}

                    var_1519:
                    if false {}

return var_1517
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) or_else(other *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = other
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1534 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1534
var_1534 = IDENTITY((self))
var var_1535 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1535

var var_1536 any
_ = var_1536
var_1536 = (*var_1534).inner
if var_1538, ok := var_1536.(Tag__none); ok {
_ = var_1538

if true {
var var_1539 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1539
var_1539 = Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((other))
var_1535 = var_1539
goto var_1537

}
}
if var_1540, ok := var_1536.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1540

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1536))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((var_1540))
if true {
var var_1541 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1541
var_1541 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((some))
var_1535 = var_1541.value
goto var_1537

}
}

                    var_1537:
                    if false {}

return var_1535
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) or_else_get(other_supplier func() *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = other_supplier
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1542 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1542
var_1542 = IDENTITY((self))
var var_1543 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1543

var var_1544 any
_ = var_1544
var_1544 = (*var_1542).inner
if var_1546, ok := var_1544.(Tag__none); ok {
_ = var_1546

if true {
var var_1547 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1547
var_1547 = other_supplier()
var_1543 = var_1547
goto var_1545

}
}
if var_1548, ok := var_1544.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1548

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1544))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((var_1548))
if true {
var var_1549 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1549
var_1549 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((some))
var_1543 = var_1549.value
goto var_1545

}
}

                    var_1545:
                    if false {}

return var_1543
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) unwrap() *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1550 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1550
var_1550 = IDENTITY((self))
var var_1551 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1551

var var_1552 any
_ = var_1552
var_1552 = (*var_1550).inner
if var_1554, ok := var_1552.(Tag__none); ok {
_ = var_1554

if true {
var var_1555 string
_ = var_1555
var_1555 = "unwrap on none"
std_____error_____panic(var_1555)
goto var_1553

}
}
if var_1556, ok := var_1552.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1556

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1552))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((var_1556))
if true {
var var_1557 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1557
var_1557 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((some))
var_1551 = var_1557.value
goto var_1553

}
}

                    var_1553:
                    if false {}

return var_1551
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) expect(msg string) *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = msg
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1558 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1558
var_1558 = IDENTITY((self))
var var_1559 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1559

var var_1560 any
_ = var_1560
var_1560 = (*var_1558).inner
if var_1562, ok := var_1560.(Tag__none); ok {
_ = var_1562

if true {
var var_1564 string
_ = var_1564
{
var var_1563 string
_ = var_1563
var_1563 = IDENTITY((msg))
var_1564 = var_1563
}
var var_1565 string
_ = var_1565
var_1565 = "expect failed: " + var_1564
std_____error_____panic(var_1565)
goto var_1561

}
}
if var_1566, ok := var_1560.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1566

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1560))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((var_1566))
if true {
var var_1567 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1567
var_1567 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((some))
var_1559 = var_1567.value
goto var_1561

}
}

                    var_1561:
                    if false {}

return var_1559
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) iter() *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_1583 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1583
{
var var_1568 bool
_ = var_1568
var_1568 = false
var consumed bool
_ = consumed
consumed = var_1568
var var_1569 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1569
var_1569 = IDENTITY((self))
var var_1570 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1570

var var_1571 any
_ = var_1571
var_1571 = (*var_1569).inner
if var_1573, ok := var_1571.(Tag__none); ok {
_ = var_1573

if true {
var var_1574 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1574
var_1574 = std_____col_____Iter_____empty_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath()
var_1570 = var_1574
goto var_1572

}
}
if var_1575, ok := var_1571.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1575

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1571))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((var_1575))
if true {
var var_1576 func() any
_ = var_1576
var_1576 = func() any {
var var_1577 bool
_ = var_1577
var_1577 = IDENTITY((consumed))
var var_1578 any
_ = var_1578
if var_1577 {
var var_1579 Tag__no_next_elem
_ = var_1579
var_1579 = Tag__no_next_elem{}
return var_1579
} else {
var var_1580 bool
_ = var_1580
var_1580 = true
consumed = var_1580
var var_1581 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1581
var_1581 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((some))
return var_1581.value
}
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_1582 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1582
var_1582 = std_____col_____Iter_____from_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath(var_1576)
var_1570 = var_1582
goto var_1572

}
}

                    var_1572:
                    if false {}

var_1583 = var_1570
}
return var_1583
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy(self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{
inner: func() any {
                    var p1 any = (self.inner)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath:
                            tmp := p1.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath)
                            _ = tmp
                            return Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }(),
}

}
type Struct_std_____col_____Iter_____Tup_int_Ref___Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath struct {
next_fn func() any
}



func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) rev() *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1601 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1601
{
var stack *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = stack
stack = std_____col_____ArrayList_____new_____Ref___Struct_std_____path_____FilePath()
var var_1584 func() any
_ = var_1584
var_1584 = func() any {
if false {goto label_1586}
label_1586:
for {
var var_1585 bool
_ = var_1585
var_1585 = true
if var_1585 {
var var_1595 **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1595
{
var var_1587 any
_ = var_1587
var_1587 = (*self).next_fn()
var var_1588 **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1588

var var_1589 any
_ = var_1589
var_1589 = var_1587
if var_1591, ok := var_1589.(**Struct_std_____path_____FilePath); ok {
_ = var_1591

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1589))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1591))
if true {
var var_1592 **Struct_std_____path_____FilePath
_ = var_1592
var_1592 = IDENTITY((t))
var var_1593 **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1593
var_1593 = stack.push(var_1592)
var_1588 = var_1593
goto var_1590

}
}
if var_1594, ok := var_1589.(Tag__no_next_elem); ok {
_ = var_1594

if true {
break label_1586
goto var_1590

}
}

                    var_1590:
                    if false {}

var_1595 = var_1588
}
} else {
break label_1586
}
}
var maybe_next *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = maybe_next
maybe_next = stack.pop()
var var_1596 bool
_ = var_1596
var_1596 = maybe_next.is_some()
var var_1597 any
_ = var_1597
if var_1596 {
var var_1598 **Struct_std_____path_____FilePath
_ = var_1598
var_1598 = maybe_next.unwrap()
return var_1598
} else {
var var_1599 Tag__no_next_elem
_ = var_1599
var_1599 = Tag__no_next_elem{}
return var_1599
}
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_1600 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1600
var_1600 = std_____col_____Iter_____from_____Ref___Struct_std_____path_____FilePath(var_1584)
var_1601 = var_1600
}
return var_1601
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) next() any {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1602 any
_ = var_1602
var_1602 = (*self).next_fn()
var var_1603 any
_ = var_1603

var var_1604 any
_ = var_1604
var_1604 = var_1602
if var_1606, ok := var_1604.(**Struct_std_____path_____FilePath); ok {
_ = var_1606

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1604))
                            _ = iface_ptr

var x **Struct_std_____path_____FilePath
_ = x
x = IDENTITY((var_1606))
if true {
var var_1607 **Struct_std_____path_____FilePath
_ = var_1607
var_1607 = IDENTITY((x))
var_1603 = var_1607
goto var_1605

}
}
if var_1608, ok := var_1604.(Tag__no_next_elem); ok {
_ = var_1608

if true {
var var_1609 Tag__no_next_elem
_ = var_1609
var_1609 = Tag__no_next_elem{}
var_1603 = var_1609
goto var_1605

}
}

                    var_1605:
                    if false {}

return var_1603
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) for_each(f func(**Struct_std_____path_____FilePath) Tup_) Tup_ {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1622 Tup_
var_1622 = Tup_{}
_ = var_1622
{
if false {goto label_1611}
label_1611:
for {
var var_1610 bool
_ = var_1610
var_1610 = true
if var_1610 {
var var_1621 Tup_
var_1621 = Tup_{}
_ = var_1621
{
var next any
_ = next
next = (*self).next_fn()
var var_1612 any
_ = var_1612
var_1612 = func() any {
                    var p1 any = (next)

                        switch p1.(type) {
                        case **Struct_std_____path_____FilePath:
                            tmp := p1.(**Struct_std_____path_____FilePath)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_1613 Tup_
var_1613 = Tup_{}
_ = var_1613

var var_1614 any
_ = var_1614
var_1614 = var_1612
if var_1616, ok := var_1614.(**Struct_std_____path_____FilePath); ok {
_ = var_1616

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1614))
                            _ = iface_ptr

var str **Struct_std_____path_____FilePath
_ = str
str = IDENTITY((var_1616))
if true {
var var_1617 **Struct_std_____path_____FilePath
_ = var_1617
var_1617 = IDENTITY((str))
var var_1618 Tup_
var_1618 = Tup_{}
_ = var_1618
var_1618 = f(var_1617)
var_1613 = var_1618
goto var_1615

}
}
if var_1619, ok := var_1614.(Tag__no_next_elem); ok {
_ = var_1619

if true {
var var_1620 Tup_
var_1620 = Tup_{}
_ = var_1620
var_1620 = Tup_{}
return var_1620
goto var_1615

}
}

                    var_1615:
                    if false {}

var_1621 = var_1613
}
} else {
break label_1611
}
}
var_1622 = Tup_{}
}
return var_1622
return Tup_{}
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) chain(other_iters any) *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = other_iters
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1623 any
_ = var_1623
var_1623 = func() any {
                    var p1 any = (other_iters)

                        switch p1.(type) {
                        case []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath:
                            tmp := p1.([]*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath)
                            _ = tmp
                            return Array_Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((tmp))
                        }

                        switch p1.(type) {
                        case *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath:
                            tmp := p1.(*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath)
                            _ = tmp
                            return Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_1624 []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1624

var var_1625 any
_ = var_1625
var_1625 = var_1623
if var_1627, ok := var_1625.([]*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1627

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1625))
                            _ = iface_ptr

var iters []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = iters
iters = Array_Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((var_1627))
if true {
var var_1628 []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1628
var_1628 = Array_Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((iters))
var_1624 = var_1628
goto var_1626

}
}
if var_1629, ok := var_1625.(*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1629

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1625))
                            _ = iface_ptr

var iter *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = iter
iter = Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((var_1629))
if true {
var var_1630 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1630
var_1630 = Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((iter))
var var_1631 []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1631
var_1631 = []*Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{var_1630}
var_1624 = var_1631
goto var_1626

}
}

                    var_1626:
                    if false {}

var remaining_iters *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = remaining_iters
remaining_iters = std_____col_____ArrayList_____from_array_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath(var_1624)
var var_1632 **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1632
var_1632 = IDENTITY((self))
var current_iter **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = current_iter
current_iter = var_1632
var var_1633 func() any
_ = var_1633
var_1633 = func() any {
if false {goto label_1635}
label_1635:
for {
var var_1634 bool
_ = var_1634
var_1634 = true
if var_1634 {
var next_elem any
_ = next_elem
next_elem = (*current_iter).next_fn()
var var_1636 any
_ = var_1636
var_1636 = func() any {
                    var p1 any = (next_elem)

                        switch p1.(type) {
                        case **Struct_std_____path_____FilePath:
                            tmp := p1.(**Struct_std_____path_____FilePath)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_1637 Tup_
var_1637 = Tup_{}
_ = var_1637

var var_1638 any
_ = var_1638
var_1638 = var_1636
if var_1640, ok := var_1638.(**Struct_std_____path_____FilePath); ok {
_ = var_1640

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1638))
                            _ = iface_ptr

var value **Struct_std_____path_____FilePath
_ = value
value = IDENTITY((var_1640))
if true {
var var_1641 **Struct_std_____path_____FilePath
_ = var_1641
var_1641 = IDENTITY((value))
return var_1641
goto var_1639

}
}
if var_1642, ok := var_1638.(Tag__no_next_elem); ok {
_ = var_1642

if true {
var var_1643 Tup_
var_1643 = Tup_{}
_ = var_1643
{
var_1643 = Tup_{}
}
var_1637 = var_1643
goto var_1639

}
}

                    var_1639:
                    if false {}

var var_1644 int
_ = var_1644
var_1644 = remaining_iters.len()
var var_1645 int
_ = var_1645
var_1645 = 0
var var_1646 bool
_ = var_1646
switch Int_Ord((var_1644), &(var_1645)).(type) {
                            case Tag__greater:
                            var_1646 = true
                            default:
                            var_1646 = false
                            }

var var_1647 Tup_
var_1647 = Tup_{}
_ = var_1647
if var_1646 {
var next_iter *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = next_iter
next_iter = remaining_iters.pop()
var var_1648 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1648
var_1648 = Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((next_iter))
var var_1649 any
_ = var_1649

var var_1650 any
_ = var_1650
var_1650 = var_1648.inner
if var_1652, ok := var_1650.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath); ok {
_ = var_1652

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1650))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy((var_1652))
if true {
var var_1653 **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1653
var_1653 = &some.value
current_iter = var_1653
continue label_1635
goto var_1651

}
}
if var_1654, ok := var_1650.(Tag__none); ok {
_ = var_1654

if true {
var var_1655 Tag__no_next_elem
_ = var_1655
var_1655 = Tag__no_next_elem{}
return var_1655
goto var_1651

}
}

                    var_1651:
                    if false {}

}
var var_1656 Tag__no_next_elem
_ = var_1656
var_1656 = Tag__no_next_elem{}
return var_1656
} else {
break label_1635
}
}
var var_1657 Tag__no_next_elem
_ = var_1657
var_1657 = Tag__no_next_elem{}
return var_1657
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_1633
var var_1658 func() any
_ = var_1658
var_1658 = IDENTITY((next_fn))
var var_1659 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1659
var_1659 = std_____col_____Iter_____from_____Ref___Struct_std_____path_____FilePath(var_1658)
return var_1659
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) skip_while(predicate func(**Struct_std_____path_____FilePath) bool) *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = predicate
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1677 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1677
{
var var_1660 func() any
_ = var_1660
var_1660 = func() any {
if false {goto label_1662}
label_1662:
for {
var var_1661 bool
_ = var_1661
var_1661 = true
if var_1661 {
var var_1663 any
_ = var_1663
var_1663 = (*self).next_fn()
var var_1664 any
_ = var_1664

var var_1665 any
_ = var_1665
var_1665 = var_1663
if var_1667, ok := var_1665.(**Struct_std_____path_____FilePath); ok {
_ = var_1667

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1665))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1667))
if true {
var var_1668 **Struct_std_____path_____FilePath
_ = var_1668
var_1668 = IDENTITY((t))
var var_1669 bool
_ = var_1669
var_1669 = predicate(var_1668)
var var_1670 any
_ = var_1670
if var_1669 {
continue label_1662
} else {
var var_1671 **Struct_std_____path_____FilePath
_ = var_1671
var_1671 = IDENTITY((t))
return var_1671
}
var_1664 = var_1670
goto var_1666

}
}
if var_1672, ok := var_1665.(Tag__no_next_elem); ok {
_ = var_1672

if true {
var var_1673 Tag__no_next_elem
_ = var_1673
var_1673 = Tag__no_next_elem{}
return var_1673
goto var_1666

}
}

                    var_1666:
                    if false {}

} else {
break label_1662
}
}
var var_1674 string
_ = var_1674
var_1674 = "skip_while"
std_____error_____unreachable(var_1674)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_1660
var var_1675 func() any
_ = var_1675
var_1675 = IDENTITY((next_fn))
var var_1676 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1676
var_1676 = std_____col_____Iter_____from_____Ref___Struct_std_____path_____FilePath(var_1675)
var_1677 = var_1676
}
return var_1677
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) take_while(predicate func(**Struct_std_____path_____FilePath) bool) *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = predicate
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1696 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1696
{
var var_1678 func() any
_ = var_1678
var_1678 = func() any {
if false {goto label_1680}
label_1680:
for {
var var_1679 bool
_ = var_1679
var_1679 = true
if var_1679 {
var var_1681 any
_ = var_1681
var_1681 = (*self).next_fn()
var var_1682 any
_ = var_1682

var var_1683 any
_ = var_1683
var_1683 = var_1681
if var_1685, ok := var_1683.(**Struct_std_____path_____FilePath); ok {
_ = var_1685

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1683))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1685))
if true {
var var_1686 **Struct_std_____path_____FilePath
_ = var_1686
var_1686 = IDENTITY((t))
var var_1687 bool
_ = var_1687
var_1687 = predicate(var_1686)
var var_1688 any
_ = var_1688
if var_1687 {
var var_1689 **Struct_std_____path_____FilePath
_ = var_1689
var_1689 = IDENTITY((t))
return var_1689
} else {
var var_1690 Tag__no_next_elem
_ = var_1690
var_1690 = Tag__no_next_elem{}
return var_1690
}
var_1682 = var_1688
goto var_1684

}
}
if var_1691, ok := var_1683.(Tag__no_next_elem); ok {
_ = var_1691

if true {
var var_1692 Tag__no_next_elem
_ = var_1692
var_1692 = Tag__no_next_elem{}
return var_1692
goto var_1684

}
}

                    var_1684:
                    if false {}

} else {
break label_1680
}
}
var var_1693 string
_ = var_1693
var_1693 = "take_while"
std_____error_____unreachable(var_1693)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_1678
var var_1694 func() any
_ = var_1694
var_1694 = IDENTITY((next_fn))
var var_1695 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1695
var_1695 = std_____col_____Iter_____from_____Ref___Struct_std_____path_____FilePath(var_1694)
var_1696 = var_1695
}
return var_1696
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) filter(predicate func(**Struct_std_____path_____FilePath) bool) *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = predicate
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1714 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1714
{
var var_1697 func() any
_ = var_1697
var_1697 = func() any {
if false {goto label_1699}
label_1699:
for {
var var_1698 bool
_ = var_1698
var_1698 = true
if var_1698 {
var var_1700 any
_ = var_1700
var_1700 = (*self).next_fn()
var var_1701 any
_ = var_1701

var var_1702 any
_ = var_1702
var_1702 = var_1700
if var_1704, ok := var_1702.(**Struct_std_____path_____FilePath); ok {
_ = var_1704

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1702))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1704))
if true {
var var_1705 **Struct_std_____path_____FilePath
_ = var_1705
var_1705 = IDENTITY((t))
var var_1706 bool
_ = var_1706
var_1706 = predicate(var_1705)
var var_1707 any
_ = var_1707
if var_1706 {
var var_1708 **Struct_std_____path_____FilePath
_ = var_1708
var_1708 = IDENTITY((t))
return var_1708
} else {
continue label_1699
}
goto var_1703

}
}
if var_1709, ok := var_1702.(Tag__no_next_elem); ok {
_ = var_1709

if true {
var var_1710 Tag__no_next_elem
_ = var_1710
var_1710 = Tag__no_next_elem{}
return var_1710
goto var_1703

}
}

                    var_1703:
                    if false {}

} else {
break label_1699
}
}
var var_1711 string
_ = var_1711
var_1711 = "filter"
std_____error_____unreachable(var_1711)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_1697
var var_1712 func() any
_ = var_1712
var_1712 = IDENTITY((next_fn))
var var_1713 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1713
var_1713 = std_____col_____Iter_____from_____Ref___Struct_std_____path_____FilePath(var_1712)
var_1714 = var_1713
}
return var_1714
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) skip(amount int) *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = amount
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1737 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1737
{
var var_1715 int
_ = var_1715
var_1715 = IDENTITY((amount))
var to_skip int
_ = to_skip
to_skip = var_1715
var var_1716 func() any
_ = var_1716
var_1716 = func() any {
if false {goto label_1718}
label_1718:
for {
var var_1717 bool
_ = var_1717
var_1717 = true
if var_1717 {
var next any
_ = next
next = (*self).next_fn()
var var_1719 int
_ = var_1719
var_1719 = IDENTITY((to_skip))
var var_1720 int
_ = var_1720
var_1720 = 0
var var_1721 bool
_ = var_1721
switch Int_Ord((var_1719), &(var_1720)).(type) {
                            case Tag__greater:
                            var_1721 = true
                            default:
                            var_1721 = false
                            }

var var_1722 Tup_
var_1722 = Tup_{}
_ = var_1722
if var_1721 {
var var_1723 int
_ = var_1723
var_1723 = IDENTITY((to_skip))
var var_1724 int
_ = var_1724
var_1724 = 1
var var_1725 int
_ = var_1725
var_1725 = var_1723 - var_1724
to_skip = var_1725
continue label_1718
}
var var_1726 any
_ = var_1726
var_1726 = func() any {
                    var p1 any = (next)

                        switch p1.(type) {
                        case **Struct_std_____path_____FilePath:
                            tmp := p1.(**Struct_std_____path_____FilePath)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_1727 **Struct_std_____path_____FilePath
_ = var_1727

var var_1728 any
_ = var_1728
var_1728 = var_1726
if var_1730, ok := var_1728.(**Struct_std_____path_____FilePath); ok {
_ = var_1730

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1728))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1730))
if true {
var var_1731 **Struct_std_____path_____FilePath
_ = var_1731
var_1731 = IDENTITY((t))
var_1727 = var_1731
goto var_1729

}
}
if var_1732, ok := var_1728.(Tag__no_next_elem); ok {
_ = var_1732

if true {
var var_1733 Tag__no_next_elem
_ = var_1733
var_1733 = Tag__no_next_elem{}
return var_1733
goto var_1729

}
}

                    var_1729:
                    if false {}

return var_1727
} else {
break label_1718
}
}
var var_1734 string
_ = var_1734
var_1734 = "skip"
std_____error_____unreachable(var_1734)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_1716
var var_1735 func() any
_ = var_1735
var_1735 = IDENTITY((next_fn))
var var_1736 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1736
var_1736 = std_____col_____Iter_____from_____Ref___Struct_std_____path_____FilePath(var_1735)
var_1737 = var_1736
}
return var_1737
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) take(amount int) *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = amount
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1758 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1758
{
var var_1738 int
_ = var_1738
var_1738 = 0
var to_take int
_ = to_take
to_take = var_1738
var var_1739 func() any
_ = var_1739
var_1739 = func() any {
var next any
_ = next
next = (*self).next_fn()
var var_1740 int
_ = var_1740
var_1740 = IDENTITY((to_take))
var var_1741 int
_ = var_1741
var_1741 = IDENTITY((amount))
var var_1742 bool
_ = var_1742
switch Int_Ord((var_1740), &(var_1741)).(type) {
                            case Tag__greater:
                            var_1742 = true
                            case Tag__equal:
                            var_1742 = true
                            default:
                            var_1742 = false
                            }

var var_1743 Tup_
var_1743 = Tup_{}
_ = var_1743
if var_1742 {
var var_1744 Tag__no_next_elem
_ = var_1744
var_1744 = Tag__no_next_elem{}
return var_1744
}
var var_1745 int
_ = var_1745
var_1745 = IDENTITY((to_take))
var var_1746 int
_ = var_1746
var_1746 = 1
var var_1747 int
_ = var_1747
var_1747 = var_1745 + var_1746
to_take = var_1747
var var_1748 any
_ = var_1748
var_1748 = func() any {
                    var p1 any = (next)

                        switch p1.(type) {
                        case **Struct_std_____path_____FilePath:
                            tmp := p1.(**Struct_std_____path_____FilePath)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_1749 **Struct_std_____path_____FilePath
_ = var_1749

var var_1750 any
_ = var_1750
var_1750 = var_1748
if var_1752, ok := var_1750.(**Struct_std_____path_____FilePath); ok {
_ = var_1752

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1750))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1752))
if true {
var var_1753 **Struct_std_____path_____FilePath
_ = var_1753
var_1753 = IDENTITY((t))
var_1749 = var_1753
goto var_1751

}
}
if var_1754, ok := var_1750.(Tag__no_next_elem); ok {
_ = var_1754

if true {
var var_1755 Tag__no_next_elem
_ = var_1755
var_1755 = Tag__no_next_elem{}
return var_1755
goto var_1751

}
}

                    var_1751:
                    if false {}

return var_1749
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_1739
var var_1756 func() any
_ = var_1756
var_1756 = IDENTITY((next_fn))
var var_1757 *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = var_1757
var_1757 = std_____col_____Iter_____from_____Ref___Struct_std_____path_____FilePath(var_1756)
var_1758 = var_1757
}
return var_1758
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) indexed() *Struct_std_____col_____Iter_____Tup_int_Ref___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1759 int
_ = var_1759
var_1759 = 0
var index int
_ = index
index = var_1759
var var_1760 func() any
_ = var_1760
var_1760 = func() any {
var var_1761 any
_ = var_1761
var_1761 = (*self).next_fn()
var var_1762 **Struct_std_____path_____FilePath
_ = var_1762

var var_1763 any
_ = var_1763
var_1763 = var_1761
if var_1765, ok := var_1763.(**Struct_std_____path_____FilePath); ok {
_ = var_1765

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1763))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1765))
if true {
var var_1766 **Struct_std_____path_____FilePath
_ = var_1766
var_1766 = IDENTITY((t))
var_1762 = var_1766
goto var_1764

}
}
if var_1767, ok := var_1763.(Tag__no_next_elem); ok {
_ = var_1767

if true {
var var_1768 Tag__no_next_elem
_ = var_1768
var_1768 = Tag__no_next_elem{}
return var_1768
goto var_1764

}
}

                    var_1764:
                    if false {}

var next **Struct_std_____path_____FilePath
_ = next
next = var_1762
var var_1769 int
_ = var_1769
var_1769 = IDENTITY((index))
var index_to_return int
_ = index_to_return
index_to_return = var_1769
var var_1770 int
_ = var_1770
var_1770 = IDENTITY((index))
var var_1771 int
_ = var_1771
var_1771 = 1
var var_1772 int
_ = var_1772
var_1772 = var_1770 + var_1771
index = var_1772
var var_1773 int
_ = var_1773
var_1773 = IDENTITY((index_to_return))
var var_1774 **Struct_std_____path_____FilePath
_ = var_1774
var_1774 = IDENTITY((next))
var var_1775 Tup_int_Ref___Struct_std_____path_____FilePath
_ = var_1775
var_1775 = Tup_int_Ref___Struct_std_____path_____FilePath{var_1773, var_1774}
return var_1775
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_1760
var var_1776 func() any
_ = var_1776
var_1776 = IDENTITY((next_fn))
var var_1777 *Struct_std_____col_____Iter_____Tup_int_Ref___Struct_std_____path_____FilePath
_ = var_1777
var_1777 = std_____col_____Iter_____from_____Tup_int_Ref___Struct_std_____path_____FilePath(var_1776)
return var_1777
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Tup_int_Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) reduce(f func(acc ***Struct_std_____path_____FilePath,elem **Struct_std_____path_____FilePath) Tup_) *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1778 any
_ = var_1778
var_1778 = (*self).next()
var var_1779 **Struct_std_____path_____FilePath
_ = var_1779

var var_1780 any
_ = var_1780
var_1780 = var_1778
if var_1782, ok := var_1780.(**Struct_std_____path_____FilePath); ok {
_ = var_1782

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1780))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1782))
if true {
var var_1783 **Struct_std_____path_____FilePath
_ = var_1783
var_1783 = IDENTITY((t))
var_1779 = var_1783
goto var_1781

}
}
if var_1784, ok := var_1780.(Tag__no_next_elem); ok {
_ = var_1784

if true {
var var_1785 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1785
var_1785 = std_____opt_____Opt_____none_____Ref___Struct_std_____path_____FilePath()
return var_1785
goto var_1781

}
}

                    var_1781:
                    if false {}

var first **Struct_std_____path_____FilePath
_ = first
first = var_1779
if false {goto label_1787}
label_1787:
for {
var var_1786 bool
_ = var_1786
var_1786 = true
if var_1786 {
var var_1799 Tup_
var_1799 = Tup_{}
_ = var_1799
{
var var_1788 any
_ = var_1788
var_1788 = (*self).next()
var var_1789 **Struct_std_____path_____FilePath
_ = var_1789

var var_1790 any
_ = var_1790
var_1790 = var_1788
if var_1792, ok := var_1790.(**Struct_std_____path_____FilePath); ok {
_ = var_1792

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1790))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1792))
if true {
var var_1793 **Struct_std_____path_____FilePath
_ = var_1793
var_1793 = IDENTITY((t))
var_1789 = var_1793
goto var_1791

}
}
if var_1794, ok := var_1790.(Tag__no_next_elem); ok {
_ = var_1794

if true {
break label_1787
goto var_1791

}
}

                    var_1791:
                    if false {}

var elem **Struct_std_____path_____FilePath
_ = elem
elem = var_1789
var var_1795 ***Struct_std_____path_____FilePath
_ = var_1795
var_1795 = &first
var var_1796 **Struct_std_____path_____FilePath
_ = var_1796
var_1796 = IDENTITY((elem))
var var_1797 Tup_
var_1797 = Tup_{}
_ = var_1797
var_1797 = f(var_1795, var_1796)
var var_1798 Tup_
var_1798 = Tup_{}
_ = var_1798
var_1798 = Tup_{}
var_1799 = var_1798
}
} else {
break label_1787
}
}
var var_1800 **Struct_std_____path_____FilePath
_ = var_1800
var_1800 = IDENTITY((first))
var var_1801 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1801
var_1801 = std_____opt_____Opt_____some_____Ref___Struct_std_____path_____FilePath(var_1800)
return var_1801
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) first() *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1802 any
_ = var_1802
var_1802 = (*self).next_fn()
var var_1803 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1803

var var_1804 any
_ = var_1804
var_1804 = var_1802
if var_1806, ok := var_1804.(**Struct_std_____path_____FilePath); ok {
_ = var_1806

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1804))
                            _ = iface_ptr

var value **Struct_std_____path_____FilePath
_ = value
value = IDENTITY((var_1806))
if true {
var var_1807 **Struct_std_____path_____FilePath
_ = var_1807
var_1807 = IDENTITY((value))
var var_1808 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1808
var_1808 = std_____opt_____Opt_____some_____Ref___Struct_std_____path_____FilePath(var_1807)
var_1803 = var_1808
goto var_1805

}
}
if var_1809, ok := var_1804.(Tag__no_next_elem); ok {
_ = var_1809

if true {
var var_1810 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1810
var_1810 = std_____opt_____Opt_____none_____Ref___Struct_std_____path_____FilePath()
var_1803 = var_1810
goto var_1805

}
}

                    var_1805:
                    if false {}

return var_1803
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) into_list() *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var arr *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = arr
arr = std_____col_____ArrayList_____new_____Ref___Struct_std_____path_____FilePath()
if false {goto label_1812}
label_1812:
for {
var var_1811 bool
_ = var_1811
var_1811 = true
if var_1811 {
var var_1822 **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1822
{
var var_1813 any
_ = var_1813
var_1813 = (*self).next_fn()
var var_1814 **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1814

var var_1815 any
_ = var_1815
var_1815 = var_1813
if var_1817, ok := var_1815.(**Struct_std_____path_____FilePath); ok {
_ = var_1817

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1815))
                            _ = iface_ptr

var elem **Struct_std_____path_____FilePath
_ = elem
elem = IDENTITY((var_1817))
if true {
var var_1818 **Struct_std_____path_____FilePath
_ = var_1818
var_1818 = IDENTITY((elem))
var var_1819 **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1819
var_1819 = arr.push(var_1818)
var_1814 = var_1819
goto var_1816

}
}
if var_1820, ok := var_1815.(Tag__no_next_elem); ok {
_ = var_1820

if true {
var var_1821 *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1821
var_1821 = Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath_Copy((arr))
return var_1821
goto var_1816

}
}

                    var_1816:
                    if false {}

var_1822 = var_1814
}
} else {
break label_1812
}
}
var var_1823 *Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
_ = var_1823
var_1823 = Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath_Copy((arr))
return var_1823
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) count() uint {

var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1842 uint
_ = var_1842
{
var var_1824 uint
_ = var_1824
var_1824 = 0
var var_1825 uint
_ = var_1825
var_1825 = uint(var_1824)
var result uint
_ = result
result = var_1825
if false {goto label_1827}
label_1827:
for {
var var_1826 bool
_ = var_1826
var_1826 = true
if var_1826 {
var var_1840 Tup_
var_1840 = Tup_{}
_ = var_1840
{
var var_1828 any
_ = var_1828
var_1828 = (*self).next_fn()
var var_1829 Tup_
var_1829 = Tup_{}
_ = var_1829

var var_1830 any
_ = var_1830
var_1830 = var_1828
if var_1832, ok := var_1830.(**Struct_std_____path_____FilePath); ok {
_ = var_1832

if true {
var var_1838 Tup_
var_1838 = Tup_{}
_ = var_1838
{
var var_1833 uint
_ = var_1833
var_1833 = IDENTITY((result))
var var_1834 uint
_ = var_1834
var_1834 = 1
var var_1835 uint
_ = var_1835
var_1835 = uint(var_1834)
var var_1836 uint
_ = var_1836
var_1836 = var_1833 + var_1835
result = var_1836
var var_1837 Tup_
var_1837 = Tup_{}
_ = var_1837
var_1837 = Tup_{}
var_1838 = var_1837
}
var_1829 = var_1838
goto var_1831

}
}
if var_1839, ok := var_1830.(Tag__no_next_elem); ok {
_ = var_1839

if true {
break label_1827
goto var_1831

}
}

                    var_1831:
                    if false {}

var_1840 = var_1829
}
} else {
break label_1827
}
}
var var_1841 uint
_ = var_1841
var_1841 = IDENTITY((result))
var_1842 = var_1841
}
return var_1842
var ΔΔΔretΔΔΔ *uint
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) find(f func(e **Struct_std_____path_____FilePath) bool) *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
if false {goto label_1844}
label_1844:
for {
var var_1843 bool
_ = var_1843
var_1843 = true
if var_1843 {
var var_1857 Tup_
var_1857 = Tup_{}
_ = var_1857
{
var var_1845 any
_ = var_1845
var_1845 = (*self).next_fn()
var var_1846 Tup_
var_1846 = Tup_{}
_ = var_1846

var var_1847 any
_ = var_1847
var_1847 = var_1845
if var_1849, ok := var_1847.(**Struct_std_____path_____FilePath); ok {
_ = var_1849

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1847))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1849))
if true {
var var_1855 Tup_
var_1855 = Tup_{}
_ = var_1855
{
var var_1850 **Struct_std_____path_____FilePath
_ = var_1850
var_1850 = IDENTITY((t))
var var_1851 bool
_ = var_1851
var_1851 = f(var_1850)
var var_1852 Tup_
var_1852 = Tup_{}
_ = var_1852
if var_1851 {
var var_1853 **Struct_std_____path_____FilePath
_ = var_1853
var_1853 = IDENTITY((t))
var var_1854 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1854
var_1854 = std_____opt_____Opt_____some_____Ref___Struct_std_____path_____FilePath(var_1853)
return var_1854
}
var_1855 = var_1852
}
var_1846 = var_1855
goto var_1848

}
}
if var_1856, ok := var_1847.(Tag__no_next_elem); ok {
_ = var_1856

if true {
break label_1844
goto var_1848

}
}

                    var_1848:
                    if false {}

var_1857 = var_1846
}
} else {
break label_1844
}
}
var var_1858 *Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
_ = var_1858
var_1858 = std_____opt_____Opt_____none_____Ref___Struct_std_____path_____FilePath()
return var_1858
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Ref___Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) any(f func(e **Struct_std_____path_____FilePath) bool) bool {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1874 bool
_ = var_1874
{
if false {goto label_1860}
label_1860:
for {
var var_1859 bool
_ = var_1859
var_1859 = true
if var_1859 {
var var_1872 Tup_
var_1872 = Tup_{}
_ = var_1872
{
var var_1861 any
_ = var_1861
var_1861 = (*self).next_fn()
var var_1862 Tup_
var_1862 = Tup_{}
_ = var_1862

var var_1863 any
_ = var_1863
var_1863 = var_1861
if var_1865, ok := var_1863.(**Struct_std_____path_____FilePath); ok {
_ = var_1865

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1863))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1865))
if true {
var var_1870 Tup_
var_1870 = Tup_{}
_ = var_1870
{
var var_1866 **Struct_std_____path_____FilePath
_ = var_1866
var_1866 = IDENTITY((t))
var var_1867 bool
_ = var_1867
var_1867 = f(var_1866)
var var_1868 Tup_
var_1868 = Tup_{}
_ = var_1868
if var_1867 {
var var_1869 bool
_ = var_1869
var_1869 = true
return var_1869
}
var_1870 = var_1868
}
var_1862 = var_1870
goto var_1864

}
}
if var_1871, ok := var_1863.(Tag__no_next_elem); ok {
_ = var_1871

if true {
break label_1860
goto var_1864

}
}

                    var_1864:
                    if false {}

var_1872 = var_1862
}
} else {
break label_1860
}
}
var var_1873 bool
_ = var_1873
var_1873 = false
var_1874 = var_1873
}
return var_1874
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) all(f func(e **Struct_std_____path_____FilePath) bool) bool {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1891 bool
_ = var_1891
{
if false {goto label_1876}
label_1876:
for {
var var_1875 bool
_ = var_1875
var_1875 = true
if var_1875 {
var var_1889 Tup_
var_1889 = Tup_{}
_ = var_1889
{
var var_1877 any
_ = var_1877
var_1877 = (*self).next_fn()
var var_1878 Tup_
var_1878 = Tup_{}
_ = var_1878

var var_1879 any
_ = var_1879
var_1879 = var_1877
if var_1881, ok := var_1879.(**Struct_std_____path_____FilePath); ok {
_ = var_1881

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1879))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1881))
if true {
var var_1887 Tup_
var_1887 = Tup_{}
_ = var_1887
{
var var_1882 **Struct_std_____path_____FilePath
_ = var_1882
var_1882 = IDENTITY((t))
var var_1883 bool
_ = var_1883
var_1883 = f(var_1882)
var var_1884 bool
_ = var_1884
var_1884 = !var_1883
var var_1885 Tup_
var_1885 = Tup_{}
_ = var_1885
if var_1884 {
var var_1886 bool
_ = var_1886
var_1886 = false
return var_1886
}
var_1887 = var_1885
}
var_1878 = var_1887
goto var_1880

}
}
if var_1888, ok := var_1879.(Tag__no_next_elem); ok {
_ = var_1888

if true {
break label_1876
goto var_1880

}
}

                    var_1880:
                    if false {}

var_1889 = var_1878
}
} else {
break label_1876
}
}
var var_1890 bool
_ = var_1890
var_1890 = true
var_1891 = var_1890
}
return var_1891
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath_Copy(self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath{
next_fn: IDENTITY((self.next_fn)),
}

}
func (duck_internal_self *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath) map_____Struct_std_____path_____FilePath(apply func(**Struct_std_____path_____FilePath) *Struct_std_____path_____FilePath) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = apply
var Δorg_addr *Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Ref___Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1904 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_1904
{
var var_1892 func() any
_ = var_1892
var_1892 = func() any {
var var_1893 any
_ = var_1893
var_1893 = (*self).next_fn()
var var_1894 any
_ = var_1894

var var_1895 any
_ = var_1895
var_1895 = var_1893
if var_1897, ok := var_1895.(**Struct_std_____path_____FilePath); ok {
_ = var_1897

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1895))
                            _ = iface_ptr

var t **Struct_std_____path_____FilePath
_ = t
t = IDENTITY((var_1897))
if true {
var var_1898 **Struct_std_____path_____FilePath
_ = var_1898
var_1898 = IDENTITY((t))
var var_1899 *Struct_std_____path_____FilePath
_ = var_1899
var_1899 = apply(var_1898)
var_1894 = var_1899
goto var_1896

}
}
if var_1900, ok := var_1895.(Tag__no_next_elem); ok {
_ = var_1900

if true {
var var_1901 Tag__no_next_elem
_ = var_1901
var_1901 = Tag__no_next_elem{}
var_1894 = var_1901
goto var_1896

}
}

                    var_1896:
                    if false {}

return var_1894
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_1892
var var_1902 func() any
_ = var_1902
var_1902 = IDENTITY((next_fn))
var var_1903 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_1903
var_1903 = std_____col_____Iter_____from_____Struct_std_____path_____FilePath(var_1902)
var_1904 = var_1903
}
return var_1904
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
type Struct_std_____opt_____Some_____string struct {
value string
}

func  Struct_std_____opt_____Some_____string_Copy(self *Struct_std_____opt_____Some_____string) *Struct_std_____opt_____Some_____string {
_ = self
return &Struct_std_____opt_____Some_____string{
value: IDENTITY((self.value)),
}

}
type Struct_std_____opt_____Opt_____string struct {
inner any
}



func (duck_internal_self *Struct_std_____opt_____Opt_____string) is_some() bool {

var self **Struct_std_____opt_____Opt_____string
_ = self
self = &duck_internal_self
var var_1905 **Struct_std_____opt_____Opt_____string
_ = var_1905
var_1905 = IDENTITY((self))
var var_1906 Tag__none
_ = var_1906
var_1906 = Tag__none{}
var var_1907 bool
_ = var_1907
var_1907 = func() bool {
                    var p1 any = ((*var_1905).inner)
                    var p2 any = (var_1906)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____string:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____string:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
var var_1908 bool
_ = var_1908
var_1908 = !var_1907
return var_1908
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____string) if_some(consumer func(value string) Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____string
_ = self
self = &duck_internal_self
var var_1918 Tup_
var_1918 = Tup_{}
_ = var_1918
{
var var_1909 **Struct_std_____opt_____Opt_____string
_ = var_1909
var_1909 = IDENTITY((self))
var var_1910 Tup_
var_1910 = Tup_{}
_ = var_1910

var var_1911 any
_ = var_1911
var_1911 = (*var_1909).inner
if var_1913, ok := var_1911.(Tag__none); ok {
_ = var_1913

if true {
var var_1914 Tup_
var_1914 = Tup_{}
_ = var_1914
var_1914 = Tup_{}
return var_1914
goto var_1912

}
}
if var_1915, ok := var_1911.(*Struct_std_____opt_____Some_____string); ok {
_ = var_1915

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1911))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____string
_ = some
some = Struct_std_____opt_____Some_____string_Copy((var_1915))
if true {
var var_1916 *Struct_std_____opt_____Some_____string
_ = var_1916
var_1916 = Struct_std_____opt_____Some_____string_Copy((some))
var var_1917 Tup_
var_1917 = Tup_{}
_ = var_1917
var_1917 = consumer(var_1916.value)
var_1910 = var_1917
goto var_1912

}
}

                    var_1912:
                    if false {}

var_1918 = var_1910
}
return var_1918
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____string) if_none(consumer func() Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____string
_ = self
self = &duck_internal_self
var var_1927 Tup_
var_1927 = Tup_{}
_ = var_1927
{
var var_1919 **Struct_std_____opt_____Opt_____string
_ = var_1919
var_1919 = IDENTITY((self))
var var_1920 Tup_
var_1920 = Tup_{}
_ = var_1920

var var_1921 any
_ = var_1921
var_1921 = (*var_1919).inner
if var_1923, ok := var_1921.(Tag__none); ok {
_ = var_1923

if true {
var var_1924 Tup_
var_1924 = Tup_{}
_ = var_1924
var_1924 = consumer()
var_1920 = var_1924
goto var_1922

}
}
if var_1925, ok := var_1921.(*Struct_std_____opt_____Some_____string); ok {
_ = var_1925

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1921))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____string
_ = some
some = Struct_std_____opt_____Some_____string_Copy((var_1925))
if true {
var var_1926 Tup_
var_1926 = Tup_{}
_ = var_1926
var_1926 = Tup_{}
return var_1926
goto var_1922

}
}

                    var_1922:
                    if false {}

var_1927 = var_1920
}
return var_1927
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____string) is_none() bool {

var self **Struct_std_____opt_____Opt_____string
_ = self
self = &duck_internal_self
var var_1928 **Struct_std_____opt_____Opt_____string
_ = var_1928
var_1928 = IDENTITY((self))
var var_1929 Tag__none
_ = var_1929
var_1929 = Tag__none{}
var var_1930 bool
_ = var_1930
var_1930 = func() bool {
                    var p1 any = ((*var_1928).inner)
                    var p2 any = (var_1929)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____string:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____string:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
return var_1930
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____string) filter(predicate func(value string) bool) *Struct_std_____opt_____Opt_____string {
_ = predicate
var self **Struct_std_____opt_____Opt_____string
_ = self
self = &duck_internal_self
var var_1931 **Struct_std_____opt_____Opt_____string
_ = var_1931
var_1931 = IDENTITY((self))
var var_1932 *Struct_std_____opt_____Opt_____string
_ = var_1932

var var_1933 any
_ = var_1933
var_1933 = (*var_1931).inner
if var_1935, ok := var_1933.(Tag__none); ok {
_ = var_1935

if true {
var var_1936 Tag__none
_ = var_1936
var_1936 = Tag__none{}
var var_1937 *Struct_std_____opt_____Opt_____string
_ = var_1937
var_1937 = &Struct_std_____opt_____Opt_____string{inner: var_1936}
var_1932 = var_1937
goto var_1934

}
}
if var_1938, ok := var_1933.(*Struct_std_____opt_____Some_____string); ok {
_ = var_1938

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1933))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____string
_ = some
some = Struct_std_____opt_____Some_____string_Copy((var_1938))
if true {
var var_1948 *Struct_std_____opt_____Opt_____string
_ = var_1948
{
var var_1939 *Struct_std_____opt_____Some_____string
_ = var_1939
var_1939 = Struct_std_____opt_____Some_____string_Copy((some))
var var_1940 bool
_ = var_1940
var_1940 = predicate(var_1939.value)
var var_1941 *Struct_std_____opt_____Opt_____string
_ = var_1941
if var_1940 {
var var_1944 *Struct_std_____opt_____Opt_____string
_ = var_1944
{
var var_1942 **Struct_std_____opt_____Opt_____string
_ = var_1942
var_1942 = IDENTITY((self))
var var_1943 *Struct_std_____opt_____Opt_____string
_ = var_1943
var_1943 = *var_1942
var_1944 = var_1943
}
var_1941 = var_1944
} else {
var var_1947 *Struct_std_____opt_____Opt_____string
_ = var_1947
{
var var_1945 Tag__none
_ = var_1945
var_1945 = Tag__none{}
var var_1946 *Struct_std_____opt_____Opt_____string
_ = var_1946
var_1946 = &Struct_std_____opt_____Opt_____string{inner: var_1945}
var_1947 = var_1946
}
var_1941 = var_1947
}
var_1948 = var_1941
}
var_1932 = var_1948
goto var_1934

}
}

                    var_1934:
                    if false {}

return var_1932
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____string) or_else(other string) string {
_ = other
var self **Struct_std_____opt_____Opt_____string
_ = self
self = &duck_internal_self
var var_1949 **Struct_std_____opt_____Opt_____string
_ = var_1949
var_1949 = IDENTITY((self))
var var_1950 string
_ = var_1950

var var_1951 any
_ = var_1951
var_1951 = (*var_1949).inner
if var_1953, ok := var_1951.(Tag__none); ok {
_ = var_1953

if true {
var var_1954 string
_ = var_1954
var_1954 = IDENTITY((other))
var_1950 = var_1954
goto var_1952

}
}
if var_1955, ok := var_1951.(*Struct_std_____opt_____Some_____string); ok {
_ = var_1955

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1951))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____string
_ = some
some = Struct_std_____opt_____Some_____string_Copy((var_1955))
if true {
var var_1956 *Struct_std_____opt_____Some_____string
_ = var_1956
var_1956 = Struct_std_____opt_____Some_____string_Copy((some))
var_1950 = var_1956.value
goto var_1952

}
}

                    var_1952:
                    if false {}

return var_1950
var ΔΔΔretΔΔΔ *string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____string) or_else_get(other_supplier func() string) string {
_ = other_supplier
var self **Struct_std_____opt_____Opt_____string
_ = self
self = &duck_internal_self
var var_1957 **Struct_std_____opt_____Opt_____string
_ = var_1957
var_1957 = IDENTITY((self))
var var_1958 string
_ = var_1958

var var_1959 any
_ = var_1959
var_1959 = (*var_1957).inner
if var_1961, ok := var_1959.(Tag__none); ok {
_ = var_1961

if true {
var var_1962 string
_ = var_1962
var_1962 = other_supplier()
var_1958 = var_1962
goto var_1960

}
}
if var_1963, ok := var_1959.(*Struct_std_____opt_____Some_____string); ok {
_ = var_1963

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1959))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____string
_ = some
some = Struct_std_____opt_____Some_____string_Copy((var_1963))
if true {
var var_1964 *Struct_std_____opt_____Some_____string
_ = var_1964
var_1964 = Struct_std_____opt_____Some_____string_Copy((some))
var_1958 = var_1964.value
goto var_1960

}
}

                    var_1960:
                    if false {}

return var_1958
var ΔΔΔretΔΔΔ *string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____string) unwrap() string {

var self **Struct_std_____opt_____Opt_____string
_ = self
self = &duck_internal_self
var var_1965 **Struct_std_____opt_____Opt_____string
_ = var_1965
var_1965 = IDENTITY((self))
var var_1966 string
_ = var_1966

var var_1967 any
_ = var_1967
var_1967 = (*var_1965).inner
if var_1969, ok := var_1967.(Tag__none); ok {
_ = var_1969

if true {
var var_1970 string
_ = var_1970
var_1970 = "unwrap on none"
std_____error_____panic(var_1970)
goto var_1968

}
}
if var_1971, ok := var_1967.(*Struct_std_____opt_____Some_____string); ok {
_ = var_1971

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1967))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____string
_ = some
some = Struct_std_____opt_____Some_____string_Copy((var_1971))
if true {
var var_1972 *Struct_std_____opt_____Some_____string
_ = var_1972
var_1972 = Struct_std_____opt_____Some_____string_Copy((some))
var_1966 = var_1972.value
goto var_1968

}
}

                    var_1968:
                    if false {}

return var_1966
var ΔΔΔretΔΔΔ *string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____string) expect(msg string) string {
_ = msg
var self **Struct_std_____opt_____Opt_____string
_ = self
self = &duck_internal_self
var var_1973 **Struct_std_____opt_____Opt_____string
_ = var_1973
var_1973 = IDENTITY((self))
var var_1974 string
_ = var_1974

var var_1975 any
_ = var_1975
var_1975 = (*var_1973).inner
if var_1977, ok := var_1975.(Tag__none); ok {
_ = var_1977

if true {
var var_1979 string
_ = var_1979
{
var var_1978 string
_ = var_1978
var_1978 = IDENTITY((msg))
var_1979 = var_1978
}
var var_1980 string
_ = var_1980
var_1980 = "expect failed: " + var_1979
std_____error_____panic(var_1980)
goto var_1976

}
}
if var_1981, ok := var_1975.(*Struct_std_____opt_____Some_____string); ok {
_ = var_1981

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1975))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____string
_ = some
some = Struct_std_____opt_____Some_____string_Copy((var_1981))
if true {
var var_1982 *Struct_std_____opt_____Some_____string
_ = var_1982
var_1982 = Struct_std_____opt_____Some_____string_Copy((some))
var_1974 = var_1982.value
goto var_1976

}
}

                    var_1976:
                    if false {}

return var_1974
var ΔΔΔretΔΔΔ *string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____string) iter() *Struct_std_____col_____Iter_____string {

var self **Struct_std_____opt_____Opt_____string
_ = self
self = &duck_internal_self
var var_1998 *Struct_std_____col_____Iter_____string
_ = var_1998
{
var var_1983 bool
_ = var_1983
var_1983 = false
var consumed bool
_ = consumed
consumed = var_1983
var var_1984 **Struct_std_____opt_____Opt_____string
_ = var_1984
var_1984 = IDENTITY((self))
var var_1985 *Struct_std_____col_____Iter_____string
_ = var_1985

var var_1986 any
_ = var_1986
var_1986 = (*var_1984).inner
if var_1988, ok := var_1986.(Tag__none); ok {
_ = var_1988

if true {
var var_1989 *Struct_std_____col_____Iter_____string
_ = var_1989
var_1989 = std_____col_____Iter_____empty_____string()
var_1985 = var_1989
goto var_1987

}
}
if var_1990, ok := var_1986.(*Struct_std_____opt_____Some_____string); ok {
_ = var_1990

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_1986))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____string
_ = some
some = Struct_std_____opt_____Some_____string_Copy((var_1990))
if true {
var var_1991 func() any
_ = var_1991
var_1991 = func() any {
var var_1992 bool
_ = var_1992
var_1992 = IDENTITY((consumed))
var var_1993 any
_ = var_1993
if var_1992 {
var var_1994 Tag__no_next_elem
_ = var_1994
var_1994 = Tag__no_next_elem{}
return var_1994
} else {
var var_1995 bool
_ = var_1995
var_1995 = true
consumed = var_1995
var var_1996 *Struct_std_____opt_____Some_____string
_ = var_1996
var_1996 = Struct_std_____opt_____Some_____string_Copy((some))
return var_1996.value
}
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_1997 *Struct_std_____col_____Iter_____string
_ = var_1997
var_1997 = std_____col_____Iter_____from_____string(var_1991)
var_1985 = var_1997
goto var_1987

}
}

                    var_1987:
                    if false {}

var_1998 = var_1985
}
return var_1998
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____string
return *ΔΔΔretΔΔΔ
}





type Struct_std_____col_____ArrayList_____string struct {
elems []string
}



func (duck_internal_self *Struct_std_____col_____ArrayList_____string) push(new_elem string) **Struct_std_____col_____ArrayList_____string {
_ = new_elem
var Δorg_addr *Struct_std_____col_____ArrayList_____string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_1999 Tup_
var_1999 = Tup_{}
_ = var_1999

            (*self).elems = append((*self).elems, new_elem)

var var_2000 **Struct_std_____col_____ArrayList_____string
_ = var_2000
var_2000 = IDENTITY((self))
return var_2000
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) pop() *Struct_std_____opt_____Opt_____string {

var Δorg_addr *Struct_std_____col_____ArrayList_____string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2001 int
_ = var_2001
var_2001 = (*self).len()
var var_2002 int
_ = var_2002
var_2002 = 0
var var_2003 bool
_ = var_2003
switch Int_Ord((var_2001), &(var_2002)).(type) {
                            case Tag__greater:
                            var_2003 = true
                            default:
                            var_2003 = false
                            }

var var_2004 Tup_
var_2004 = Tup_{}
_ = var_2004
if var_2003 {
var var_2005 string
_ = var_2005

                var index = len((*self).elems) - 1;
                var_2005 = (*self).elems[index];
                (*self).elems = (*self).elems[:index];

var var_2006 string
_ = var_2006
var_2006 = var_2005
var value string
_ = value
value = var_2006
var var_2007 string
_ = var_2007
var_2007 = IDENTITY((value))
var var_2008 *Struct_std_____opt_____Opt_____string
_ = var_2008
var_2008 = std_____opt_____Opt_____some_____string(var_2007)
return var_2008
}
var var_2009 *Struct_std_____opt_____Opt_____string
_ = var_2009
var_2009 = std_____opt_____Opt_____none_____string()
return var_2009
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) get(index int) *Struct_std_____opt_____Opt_____string {
_ = index
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
var tag_none *Struct_std_____opt_____Opt_____string
_ = tag_none
tag_none = std_____opt_____Opt_____none_____string()
var var_2010 string
_ = var_2010

            if index < 0 || index >= (*self).len() {
                return tag_none;
            }

            var_2010 = (*self).elems[index]

var var_2011 string
_ = var_2011
var_2011 = var_2010
var elem string
_ = elem
elem = var_2011
var var_2012 string
_ = var_2012
var_2012 = IDENTITY((elem))
var var_2013 *Struct_std_____opt_____Opt_____string
_ = var_2013
var_2013 = std_____opt_____Opt_____some_____string(var_2012)
return var_2013
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) set(index int, value string) **Struct_std_____col_____ArrayList_____string {
_ = index
_ = value
var Δorg_addr *Struct_std_____col_____ArrayList_____string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2014 Tup_
var_2014 = Tup_{}
_ = var_2014

            (*self).elems[index] = value

var var_2015 **Struct_std_____col_____ArrayList_____string
_ = var_2015
var_2015 = IDENTITY((self))
return var_2015
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) remove(index int) **Struct_std_____col_____ArrayList_____string {
_ = index
var Δorg_addr *Struct_std_____col_____ArrayList_____string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2016 Tup_
var_2016 = Tup_{}
_ = var_2016

            (*self).elems = append((*self).elems[:index], (*self).elems[index+1:]...)

var var_2017 **Struct_std_____col_____ArrayList_____string
_ = var_2017
var_2017 = IDENTITY((self))
return var_2017
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) len() int {

var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
var var_2018 int
_ = var_2018
var_2018 = 0
var l int
_ = l
l = var_2018
var var_2019 Tup_
var_2019 = Tup_{}
_ = var_2019

            l = len((*self).elems);

var var_2020 int
_ = var_2020
var_2020 = IDENTITY((l))
return var_2020
var ΔΔΔretΔΔΔ *int
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) is_empty() bool {

var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
var var_2021 bool
_ = var_2021
var_2021 = true
var empty bool
_ = empty
empty = var_2021
var var_2022 Tup_
var_2022 = Tup_{}
_ = var_2022

            empty = len((*self).elems) == 0

var var_2023 bool
_ = var_2023
var_2023 = IDENTITY((empty))
return var_2023
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) clear() **Struct_std_____col_____ArrayList_____string {

var Δorg_addr *Struct_std_____col_____ArrayList_____string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2024 []string
_ = var_2024
var_2024 = []string{}
var var_2025 []string
_ = var_2025
var_2025 = var_2024
(*self).elems = var_2025
var var_2026 **Struct_std_____col_____ArrayList_____string
_ = var_2026
var_2026 = IDENTITY((self))
return var_2026
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) filter(f func(x string) bool) *Struct_std_____col_____ArrayList_____string {
_ = f
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
var res *Struct_std_____col_____ArrayList_____string
_ = res
res = std_____col_____ArrayList_____new_____string()
var var_2027 Tup_
var_2027 = Tup_{}
_ = var_2027

            for _, e := range (*self).elems {
                if f(e) {
                    res.elems = append(res.elems, e)
                }
            }

var var_2028 *Struct_std_____col_____ArrayList_____string
_ = var_2028
var_2028 = Struct_std_____col_____ArrayList_____string_Copy((res))
return var_2028
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) for_each(f func(e string) Tup_) Tup_ {
_ = f
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
var var_2030 Tup_
var_2030 = Tup_{}
_ = var_2030
{
var var_2029 Tup_
var_2029 = Tup_{}
_ = var_2029

            for _, e := range (*self).elems {
                f(e)
            }

var_2030 = var_2029
}
return var_2030
return Tup_{}
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) find(f func(x string) bool) any {
_ = f
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
var var_2031 Tag__none
_ = var_2031
var_2031 = Tag__none{}
var result any
_ = result
result = var_2031
var var_2032 Tup_
var_2032 = Tup_{}
_ = var_2032

            for _, e := range (*self).elems {
                if f(e) {
                    result = e
                    break
                }
            }

var var_2033 any
_ = var_2033
var_2033 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case string:
                            tmp := p1.(string)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_2033
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) contains(something string) bool {
_ = something
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
var var_2034 bool
_ = var_2034
var_2034 = false
var result bool
_ = result
result = var_2034
var var_2035 Tup_
var_2035 = Tup_{}
_ = var_2035

            for _, e := range (*self).elems {
                if e == something {
                    result = true
                    break
                }
            }

var var_2036 bool
_ = var_2036
var_2036 = IDENTITY((result))
return var_2036
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) any(f func(x string) bool) bool {
_ = f
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
var var_2037 bool
_ = var_2037
var_2037 = false
var result bool
_ = result
result = var_2037
var var_2038 Tup_
var_2038 = Tup_{}
_ = var_2038

            for _, e := range (*self).elems {
                if f(e) {
                    result = true
                    break
                }
            }

var var_2039 bool
_ = var_2039
var_2039 = IDENTITY((result))
return var_2039
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) find_index(predicate func(element string) bool) any {
_ = predicate
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
var var_2040 Tag__none
_ = var_2040
var_2040 = Tag__none{}
var result any
_ = result
result = var_2040
var var_2041 Tup_
var_2041 = Tup_{}
_ = var_2041

            for i, elem := range (*self).elems {
                if predicate(elem) {
                    result = i;
                    break
                }
            }

var var_2042 any
_ = var_2042
var_2042 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case int:
                            tmp := p1.(int)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_2042
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) iter() *Struct_std_____col_____Iter_____Ref___string {

var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
var var_2043 int
_ = var_2043
var_2043 = 0
var idx int
_ = idx
idx = var_2043
var var_2044 func() any
_ = var_2044
var_2044 = func() any {
var var_2045 int
_ = var_2045
var_2045 = IDENTITY((idx))
var var_2046 int
_ = var_2046
var_2046 = (*self).len()
var var_2047 bool
_ = var_2047
switch Int_Ord((var_2045), &(var_2046)).(type) {
                            case Tag__greater:
                            var_2047 = true
                            case Tag__equal:
                            var_2047 = true
                            default:
                            var_2047 = false
                            }

var var_2048 Tup_
var_2048 = Tup_{}
_ = var_2048
if var_2047 {
var var_2049 Tag__no_next_elem
_ = var_2049
var_2049 = Tag__no_next_elem{}
return var_2049
}
var var_2050 int
_ = var_2050
var_2050 = IDENTITY((idx))
var var_2051 *string
_ = var_2051
var_2051 = &(*self).elems[var_2050]
var elem_to_ret *string
_ = elem_to_ret
elem_to_ret = var_2051
var var_2052 int
_ = var_2052
var_2052 = IDENTITY((idx))
var var_2053 int
_ = var_2053
var_2053 = 1
var var_2054 int
_ = var_2054
var_2054 = var_2052 + var_2053
idx = var_2054
var var_2055 *string
_ = var_2055
var_2055 = IDENTITY((elem_to_ret))
return var_2055
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_2044
var var_2056 func() any
_ = var_2056
var_2056 = IDENTITY((f))
var var_2057 *Struct_std_____col_____Iter_____Ref___string
_ = var_2057
var_2057 = std_____col_____Iter_____from_____Ref___string(var_2056)
return var_2057
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) iter_mut() *Struct_std_____col_____Iter_____RefMut___string {

var Δorg_addr *Struct_std_____col_____ArrayList_____string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2058 int
_ = var_2058
var_2058 = 0
var idx int
_ = idx
idx = var_2058
var var_2059 func() any
_ = var_2059
var_2059 = func() any {
var var_2060 int
_ = var_2060
var_2060 = IDENTITY((idx))
var var_2061 int
_ = var_2061
var_2061 = (*self).len()
var var_2062 bool
_ = var_2062
switch Int_Ord((var_2060), &(var_2061)).(type) {
                            case Tag__greater:
                            var_2062 = true
                            case Tag__equal:
                            var_2062 = true
                            default:
                            var_2062 = false
                            }

var var_2063 Tup_
var_2063 = Tup_{}
_ = var_2063
if var_2062 {
var var_2064 Tag__no_next_elem
_ = var_2064
var_2064 = Tag__no_next_elem{}
return var_2064
}
var var_2065 int
_ = var_2065
var_2065 = IDENTITY((idx))
var var_2066 *string
_ = var_2066
var_2066 = &(*self).elems[var_2065]
var elem_to_ret *string
_ = elem_to_ret
elem_to_ret = var_2066
var var_2067 int
_ = var_2067
var_2067 = IDENTITY((idx))
var var_2068 int
_ = var_2068
var_2068 = 1
var var_2069 int
_ = var_2069
var_2069 = var_2067 + var_2068
idx = var_2069
var var_2070 *string
_ = var_2070
var_2070 = IDENTITY((elem_to_ret))
return var_2070
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_2059
var var_2071 func() any
_ = var_2071
var_2071 = IDENTITY((f))
var var_2072 *Struct_std_____col_____Iter_____RefMut___string
_ = var_2072
var_2072 = std_____col_____Iter_____from_____RefMut___string(var_2071)
return var_2072
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) as_ref() *[]string {

var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
var var_2073 *[]string
_ = var_2073
var_2073 = &(*self).elems
return var_2073
var ΔΔΔretΔΔΔ **[]string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____string) as_mut() *[]string {

var Δorg_addr *Struct_std_____col_____ArrayList_____string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2074 *[]string
_ = var_2074
var_2074 = &(*self).elems
return var_2074
var ΔΔΔretΔΔΔ **[]string
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____col_____ArrayList_____string_Copy(self *Struct_std_____col_____ArrayList_____string) *Struct_std_____col_____ArrayList_____string {
_ = self
return &Struct_std_____col_____ArrayList_____string{
elems: Array_string_Copy((self.elems)),
}

}
func (duck_internal_self *Struct_std_____col_____ArrayList_____string) map_____string(f func(x string) string) *Struct_std_____col_____ArrayList_____string {
_ = f
var Δorg_addr *Struct_std_____col_____ArrayList_____string
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____string
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var res *Struct_std_____col_____ArrayList_____string
_ = res
res = std_____col_____ArrayList_____new_____string()
var var_2075 Tup_
var_2075 = Tup_{}
_ = var_2075

            for _, e := range (*self).elems {
                res.elems = append(res.elems, f(e))
            }

var var_2076 *Struct_std_____col_____ArrayList_____string
_ = var_2076
var_2076 = Struct_std_____col_____ArrayList_____string_Copy((res))
return var_2076
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____string
return *ΔΔΔretΔΔΔ
}
type Struct_std_____opt_____Some_____Struct_std_____path_____FilePath struct {
value *Struct_std_____path_____FilePath
}

func  Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy(self *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath) *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____opt_____Some_____Struct_std_____path_____FilePath{
value: Struct_std_____path_____FilePath_Copy((self.value)),
}

}
type Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath struct {
inner any
}



func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath) is_some() bool {

var self **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2077 **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2077
var_2077 = IDENTITY((self))
var var_2078 Tag__none
_ = var_2078
var_2078 = Tag__none{}
var var_2079 bool
_ = var_2079
var_2079 = func() bool {
                    var p1 any = ((*var_2077).inner)
                    var p2 any = (var_2078)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
var var_2080 bool
_ = var_2080
var_2080 = !var_2079
return var_2080
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath) if_some(consumer func(value *Struct_std_____path_____FilePath) Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2090 Tup_
var_2090 = Tup_{}
_ = var_2090
{
var var_2081 **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2081
var_2081 = IDENTITY((self))
var var_2082 Tup_
var_2082 = Tup_{}
_ = var_2082

var var_2083 any
_ = var_2083
var_2083 = (*var_2081).inner
if var_2085, ok := var_2083.(Tag__none); ok {
_ = var_2085

if true {
var var_2086 Tup_
var_2086 = Tup_{}
_ = var_2086
var_2086 = Tup_{}
return var_2086
goto var_2084

}
}
if var_2087, ok := var_2083.(*Struct_std_____opt_____Some_____Struct_std_____path_____FilePath); ok {
_ = var_2087

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2083))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((var_2087))
if true {
var var_2088 *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = var_2088
var_2088 = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((some))
var var_2089 Tup_
var_2089 = Tup_{}
_ = var_2089
var_2089 = consumer(var_2088.value)
var_2082 = var_2089
goto var_2084

}
}

                    var_2084:
                    if false {}

var_2090 = var_2082
}
return var_2090
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath) if_none(consumer func() Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2099 Tup_
var_2099 = Tup_{}
_ = var_2099
{
var var_2091 **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2091
var_2091 = IDENTITY((self))
var var_2092 Tup_
var_2092 = Tup_{}
_ = var_2092

var var_2093 any
_ = var_2093
var_2093 = (*var_2091).inner
if var_2095, ok := var_2093.(Tag__none); ok {
_ = var_2095

if true {
var var_2096 Tup_
var_2096 = Tup_{}
_ = var_2096
var_2096 = consumer()
var_2092 = var_2096
goto var_2094

}
}
if var_2097, ok := var_2093.(*Struct_std_____opt_____Some_____Struct_std_____path_____FilePath); ok {
_ = var_2097

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2093))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((var_2097))
if true {
var var_2098 Tup_
var_2098 = Tup_{}
_ = var_2098
var_2098 = Tup_{}
return var_2098
goto var_2094

}
}

                    var_2094:
                    if false {}

var_2099 = var_2092
}
return var_2099
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath) is_none() bool {

var self **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2100 **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2100
var_2100 = IDENTITY((self))
var var_2101 Tag__none
_ = var_2101
var_2101 = Tag__none{}
var var_2102 bool
_ = var_2102
var_2102 = func() bool {
                    var p1 any = ((*var_2100).inner)
                    var p2 any = (var_2101)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
return var_2102
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath) filter(predicate func(value *Struct_std_____path_____FilePath) bool) *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath {
_ = predicate
var self **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2103 **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2103
var_2103 = IDENTITY((self))
var var_2104 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2104

var var_2105 any
_ = var_2105
var_2105 = (*var_2103).inner
if var_2107, ok := var_2105.(Tag__none); ok {
_ = var_2107

if true {
var var_2108 Tag__none
_ = var_2108
var_2108 = Tag__none{}
var var_2109 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2109
var_2109 = &Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath{inner: var_2108}
var_2104 = var_2109
goto var_2106

}
}
if var_2110, ok := var_2105.(*Struct_std_____opt_____Some_____Struct_std_____path_____FilePath); ok {
_ = var_2110

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2105))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((var_2110))
if true {
var var_2120 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2120
{
var var_2111 *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = var_2111
var_2111 = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((some))
var var_2112 bool
_ = var_2112
var_2112 = predicate(var_2111.value)
var var_2113 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2113
if var_2112 {
var var_2116 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2116
{
var var_2114 **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2114
var_2114 = IDENTITY((self))
var var_2115 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2115
var_2115 = *var_2114
var_2116 = var_2115
}
var_2113 = var_2116
} else {
var var_2119 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2119
{
var var_2117 Tag__none
_ = var_2117
var_2117 = Tag__none{}
var var_2118 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2118
var_2118 = &Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath{inner: var_2117}
var_2119 = var_2118
}
var_2113 = var_2119
}
var_2120 = var_2113
}
var_2104 = var_2120
goto var_2106

}
}

                    var_2106:
                    if false {}

return var_2104
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath) or_else(other *Struct_std_____path_____FilePath) *Struct_std_____path_____FilePath {
_ = other
var self **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2121 **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2121
var_2121 = IDENTITY((self))
var var_2122 *Struct_std_____path_____FilePath
_ = var_2122

var var_2123 any
_ = var_2123
var_2123 = (*var_2121).inner
if var_2125, ok := var_2123.(Tag__none); ok {
_ = var_2125

if true {
var var_2126 *Struct_std_____path_____FilePath
_ = var_2126
var_2126 = Struct_std_____path_____FilePath_Copy((other))
var_2122 = var_2126
goto var_2124

}
}
if var_2127, ok := var_2123.(*Struct_std_____opt_____Some_____Struct_std_____path_____FilePath); ok {
_ = var_2127

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2123))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((var_2127))
if true {
var var_2128 *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = var_2128
var_2128 = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((some))
var_2122 = var_2128.value
goto var_2124

}
}

                    var_2124:
                    if false {}

return var_2122
var ΔΔΔretΔΔΔ **Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath) or_else_get(other_supplier func() *Struct_std_____path_____FilePath) *Struct_std_____path_____FilePath {
_ = other_supplier
var self **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2129 **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2129
var_2129 = IDENTITY((self))
var var_2130 *Struct_std_____path_____FilePath
_ = var_2130

var var_2131 any
_ = var_2131
var_2131 = (*var_2129).inner
if var_2133, ok := var_2131.(Tag__none); ok {
_ = var_2133

if true {
var var_2134 *Struct_std_____path_____FilePath
_ = var_2134
var_2134 = other_supplier()
var_2130 = var_2134
goto var_2132

}
}
if var_2135, ok := var_2131.(*Struct_std_____opt_____Some_____Struct_std_____path_____FilePath); ok {
_ = var_2135

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2131))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((var_2135))
if true {
var var_2136 *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = var_2136
var_2136 = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((some))
var_2130 = var_2136.value
goto var_2132

}
}

                    var_2132:
                    if false {}

return var_2130
var ΔΔΔretΔΔΔ **Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath) unwrap() *Struct_std_____path_____FilePath {

var self **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2137 **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2137
var_2137 = IDENTITY((self))
var var_2138 *Struct_std_____path_____FilePath
_ = var_2138

var var_2139 any
_ = var_2139
var_2139 = (*var_2137).inner
if var_2141, ok := var_2139.(Tag__none); ok {
_ = var_2141

if true {
var var_2142 string
_ = var_2142
var_2142 = "unwrap on none"
std_____error_____panic(var_2142)
goto var_2140

}
}
if var_2143, ok := var_2139.(*Struct_std_____opt_____Some_____Struct_std_____path_____FilePath); ok {
_ = var_2143

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2139))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((var_2143))
if true {
var var_2144 *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = var_2144
var_2144 = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((some))
var_2138 = var_2144.value
goto var_2140

}
}

                    var_2140:
                    if false {}

return var_2138
var ΔΔΔretΔΔΔ **Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath) expect(msg string) *Struct_std_____path_____FilePath {
_ = msg
var self **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2145 **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2145
var_2145 = IDENTITY((self))
var var_2146 *Struct_std_____path_____FilePath
_ = var_2146

var var_2147 any
_ = var_2147
var_2147 = (*var_2145).inner
if var_2149, ok := var_2147.(Tag__none); ok {
_ = var_2149

if true {
var var_2151 string
_ = var_2151
{
var var_2150 string
_ = var_2150
var_2150 = IDENTITY((msg))
var_2151 = var_2150
}
var var_2152 string
_ = var_2152
var_2152 = "expect failed: " + var_2151
std_____error_____panic(var_2152)
goto var_2148

}
}
if var_2153, ok := var_2147.(*Struct_std_____opt_____Some_____Struct_std_____path_____FilePath); ok {
_ = var_2153

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2147))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((var_2153))
if true {
var var_2154 *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = var_2154
var_2154 = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((some))
var_2146 = var_2154.value
goto var_2148

}
}

                    var_2148:
                    if false {}

return var_2146
var ΔΔΔretΔΔΔ **Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath) iter() *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var self **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2170 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2170
{
var var_2155 bool
_ = var_2155
var_2155 = false
var consumed bool
_ = consumed
consumed = var_2155
var var_2156 **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2156
var_2156 = IDENTITY((self))
var var_2157 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2157

var var_2158 any
_ = var_2158
var_2158 = (*var_2156).inner
if var_2160, ok := var_2158.(Tag__none); ok {
_ = var_2160

if true {
var var_2161 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2161
var_2161 = std_____col_____Iter_____empty_____Struct_std_____path_____FilePath()
var_2157 = var_2161
goto var_2159

}
}
if var_2162, ok := var_2158.(*Struct_std_____opt_____Some_____Struct_std_____path_____FilePath); ok {
_ = var_2162

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2158))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((var_2162))
if true {
var var_2163 func() any
_ = var_2163
var_2163 = func() any {
var var_2164 bool
_ = var_2164
var_2164 = IDENTITY((consumed))
var var_2165 any
_ = var_2165
if var_2164 {
var var_2166 Tag__no_next_elem
_ = var_2166
var_2166 = Tag__no_next_elem{}
return var_2166
} else {
var var_2167 bool
_ = var_2167
var_2167 = true
consumed = var_2167
var var_2168 *Struct_std_____opt_____Some_____Struct_std_____path_____FilePath
_ = var_2168
var_2168 = Struct_std_____opt_____Some_____Struct_std_____path_____FilePath_Copy((some))
return var_2168.value
}
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_2169 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2169
var_2169 = std_____col_____Iter_____from_____Struct_std_____path_____FilePath(var_2163)
var_2157 = var_2169
goto var_2159

}
}

                    var_2159:
                    if false {}

var_2170 = var_2157
}
return var_2170
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

type Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath struct {
elems []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
}



func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) push(new_elem *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = new_elem
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2171 Tup_
var_2171 = Tup_{}
_ = var_2171

            (*self).elems = append((*self).elems, new_elem)

var var_2172 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2172
var_2172 = IDENTITY((self))
return var_2172
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) pop() *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2173 int
_ = var_2173
var_2173 = (*self).len()
var var_2174 int
_ = var_2174
var_2174 = 0
var var_2175 bool
_ = var_2175
switch Int_Ord((var_2173), &(var_2174)).(type) {
                            case Tag__greater:
                            var_2175 = true
                            default:
                            var_2175 = false
                            }

var var_2176 Tup_
var_2176 = Tup_{}
_ = var_2176
if var_2175 {
var var_2177 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2177

                var index = len((*self).elems) - 1;
                var_2177 = (*self).elems[index];
                (*self).elems = (*self).elems[:index];

var var_2178 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2178
var_2178 = var_2177
var value *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = value
value = var_2178
var var_2179 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2179
var_2179 = Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((value))
var var_2180 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2180
var_2180 = std_____opt_____Opt_____some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath(var_2179)
return var_2180
}
var var_2181 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2181
var_2181 = std_____opt_____Opt_____none_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath()
return var_2181
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) get(index int) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = index
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var tag_none *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = tag_none
tag_none = std_____opt_____Opt_____none_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath()
var var_2182 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2182

            if index < 0 || index >= (*self).len() {
                return tag_none;
            }

            var_2182 = (*self).elems[index]

var var_2183 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2183
var_2183 = var_2182
var elem *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = elem
elem = var_2183
var var_2184 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2184
var_2184 = Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((elem))
var var_2185 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2185
var_2185 = std_____opt_____Opt_____some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath(var_2184)
return var_2185
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) set(index int, value *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = index
_ = value
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2186 Tup_
var_2186 = Tup_{}
_ = var_2186

            (*self).elems[index] = value

var var_2187 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2187
var_2187 = IDENTITY((self))
return var_2187
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) remove(index int) **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = index
var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2188 Tup_
var_2188 = Tup_{}
_ = var_2188

            (*self).elems = append((*self).elems[:index], (*self).elems[index+1:]...)

var var_2189 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2189
var_2189 = IDENTITY((self))
return var_2189
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) len() int {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2190 int
_ = var_2190
var_2190 = 0
var l int
_ = l
l = var_2190
var var_2191 Tup_
var_2191 = Tup_{}
_ = var_2191

            l = len((*self).elems);

var var_2192 int
_ = var_2192
var_2192 = IDENTITY((l))
return var_2192
var ΔΔΔretΔΔΔ *int
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) is_empty() bool {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2193 bool
_ = var_2193
var_2193 = true
var empty bool
_ = empty
empty = var_2193
var var_2194 Tup_
var_2194 = Tup_{}
_ = var_2194

            empty = len((*self).elems) == 0

var var_2195 bool
_ = var_2195
var_2195 = IDENTITY((empty))
return var_2195
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) clear() **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2196 []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2196
var_2196 = []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{}
var var_2197 []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2197
var_2197 = var_2196
(*self).elems = var_2197
var var_2198 **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2198
var_2198 = IDENTITY((self))
return var_2198
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) filter(f func(x *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) bool) *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var res *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = res
res = std_____col_____ArrayList_____new_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath()
var var_2199 Tup_
var_2199 = Tup_{}
_ = var_2199

            for _, e := range (*self).elems {
                if f(e) {
                    res.elems = append(res.elems, e)
                }
            }

var var_2200 *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2200
var_2200 = Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((res))
return var_2200
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) for_each(f func(e *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) Tup_) Tup_ {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2202 Tup_
var_2202 = Tup_{}
_ = var_2202
{
var var_2201 Tup_
var_2201 = Tup_{}
_ = var_2201

            for _, e := range (*self).elems {
                f(e)
            }

var_2202 = var_2201
}
return var_2202
return Tup_{}
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) find(f func(x *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) bool) any {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2203 Tag__none
_ = var_2203
var_2203 = Tag__none{}
var result any
_ = result
result = var_2203
var var_2204 Tup_
var_2204 = Tup_{}
_ = var_2204

            for _, e := range (*self).elems {
                if f(e) {
                    result = e
                    break
                }
            }

var var_2205 any
_ = var_2205
var_2205 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath:
                            tmp := p1.(*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath)
                            _ = tmp
                            return Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_2205
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) contains(something *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) bool {
_ = something
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2206 bool
_ = var_2206
var_2206 = false
var result bool
_ = result
result = var_2206
var var_2207 Tup_
var_2207 = Tup_{}
_ = var_2207

            for _, e := range (*self).elems {
                if e == something {
                    result = true
                    break
                }
            }

var var_2208 bool
_ = var_2208
var_2208 = IDENTITY((result))
return var_2208
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) any(f func(x *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) bool) bool {
_ = f
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2209 bool
_ = var_2209
var_2209 = false
var result bool
_ = result
result = var_2209
var var_2210 Tup_
var_2210 = Tup_{}
_ = var_2210

            for _, e := range (*self).elems {
                if f(e) {
                    result = true
                    break
                }
            }

var var_2211 bool
_ = var_2211
var_2211 = IDENTITY((result))
return var_2211
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) find_index(predicate func(element *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) bool) any {
_ = predicate
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2212 Tag__none
_ = var_2212
var_2212 = Tag__none{}
var result any
_ = result
result = var_2212
var var_2213 Tup_
var_2213 = Tup_{}
_ = var_2213

            for i, elem := range (*self).elems {
                if predicate(elem) {
                    result = i;
                    break
                }
            }

var var_2214 any
_ = var_2214
var_2214 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case int:
                            tmp := p1.(int)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_2214
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) iter() *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2215 int
_ = var_2215
var_2215 = 0
var idx int
_ = idx
idx = var_2215
var var_2216 func() any
_ = var_2216
var_2216 = func() any {
var var_2217 int
_ = var_2217
var_2217 = IDENTITY((idx))
var var_2218 int
_ = var_2218
var_2218 = (*self).len()
var var_2219 bool
_ = var_2219
switch Int_Ord((var_2217), &(var_2218)).(type) {
                            case Tag__greater:
                            var_2219 = true
                            case Tag__equal:
                            var_2219 = true
                            default:
                            var_2219 = false
                            }

var var_2220 Tup_
var_2220 = Tup_{}
_ = var_2220
if var_2219 {
var var_2221 Tag__no_next_elem
_ = var_2221
var_2221 = Tag__no_next_elem{}
return var_2221
}
var var_2222 int
_ = var_2222
var_2222 = IDENTITY((idx))
var var_2223 **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2223
var_2223 = &(*self).elems[var_2222]
var elem_to_ret **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = elem_to_ret
elem_to_ret = var_2223
var var_2224 int
_ = var_2224
var_2224 = IDENTITY((idx))
var var_2225 int
_ = var_2225
var_2225 = 1
var var_2226 int
_ = var_2226
var_2226 = var_2224 + var_2225
idx = var_2226
var var_2227 **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2227
var_2227 = IDENTITY((elem_to_ret))
return var_2227
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_2216
var var_2228 func() any
_ = var_2228
var_2228 = IDENTITY((f))
var var_2229 *Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2229
var_2229 = std_____col_____Iter_____from_____Ref___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath(var_2228)
return var_2229
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) iter_mut() *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2230 int
_ = var_2230
var_2230 = 0
var idx int
_ = idx
idx = var_2230
var var_2231 func() any
_ = var_2231
var_2231 = func() any {
var var_2232 int
_ = var_2232
var_2232 = IDENTITY((idx))
var var_2233 int
_ = var_2233
var_2233 = (*self).len()
var var_2234 bool
_ = var_2234
switch Int_Ord((var_2232), &(var_2233)).(type) {
                            case Tag__greater:
                            var_2234 = true
                            case Tag__equal:
                            var_2234 = true
                            default:
                            var_2234 = false
                            }

var var_2235 Tup_
var_2235 = Tup_{}
_ = var_2235
if var_2234 {
var var_2236 Tag__no_next_elem
_ = var_2236
var_2236 = Tag__no_next_elem{}
return var_2236
}
var var_2237 int
_ = var_2237
var_2237 = IDENTITY((idx))
var var_2238 **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2238
var_2238 = &(*self).elems[var_2237]
var elem_to_ret **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = elem_to_ret
elem_to_ret = var_2238
var var_2239 int
_ = var_2239
var_2239 = IDENTITY((idx))
var var_2240 int
_ = var_2240
var_2240 = 1
var var_2241 int
_ = var_2241
var_2241 = var_2239 + var_2240
idx = var_2241
var var_2242 **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2242
var_2242 = IDENTITY((elem_to_ret))
return var_2242
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_2231
var var_2243 func() any
_ = var_2243
var_2243 = IDENTITY((f))
var var_2244 *Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2244
var_2244 = std_____col_____Iter_____from_____RefMut___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath(var_2243)
return var_2244
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) as_ref() *[]*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2245 *[]*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2245
var_2245 = &(*self).elems
return var_2245
var ΔΔΔretΔΔΔ **[]*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) as_mut() *[]*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2246 *[]*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2246
var_2246 = &(*self).elems
return var_2246
var ΔΔΔretΔΔΔ **[]*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy(self *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{
elems: Array_Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((self.elems)),
}

}
type Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath struct {
value *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
}

func  Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy(self *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{
value: Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((self.value)),
}

}
type Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath struct {
inner any
}



func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) is_some() bool {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2247 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2247
var_2247 = IDENTITY((self))
var var_2248 Tag__none
_ = var_2248
var_2248 = Tag__none{}
var var_2249 bool
_ = var_2249
var_2249 = func() bool {
                    var p1 any = ((*var_2247).inner)
                    var p2 any = (var_2248)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
var var_2250 bool
_ = var_2250
var_2250 = !var_2249
return var_2250
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) if_some(consumer func(value *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2260 Tup_
var_2260 = Tup_{}
_ = var_2260
{
var var_2251 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2251
var_2251 = IDENTITY((self))
var var_2252 Tup_
var_2252 = Tup_{}
_ = var_2252

var var_2253 any
_ = var_2253
var_2253 = (*var_2251).inner
if var_2255, ok := var_2253.(Tag__none); ok {
_ = var_2255

if true {
var var_2256 Tup_
var_2256 = Tup_{}
_ = var_2256
var_2256 = Tup_{}
return var_2256
goto var_2254

}
}
if var_2257, ok := var_2253.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath); ok {
_ = var_2257

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2253))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((var_2257))
if true {
var var_2258 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2258
var_2258 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((some))
var var_2259 Tup_
var_2259 = Tup_{}
_ = var_2259
var_2259 = consumer(var_2258.value)
var_2252 = var_2259
goto var_2254

}
}

                    var_2254:
                    if false {}

var_2260 = var_2252
}
return var_2260
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) if_none(consumer func() Tup_) Tup_ {
_ = consumer
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2269 Tup_
var_2269 = Tup_{}
_ = var_2269
{
var var_2261 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2261
var_2261 = IDENTITY((self))
var var_2262 Tup_
var_2262 = Tup_{}
_ = var_2262

var var_2263 any
_ = var_2263
var_2263 = (*var_2261).inner
if var_2265, ok := var_2263.(Tag__none); ok {
_ = var_2265

if true {
var var_2266 Tup_
var_2266 = Tup_{}
_ = var_2266
var_2266 = consumer()
var_2262 = var_2266
goto var_2264

}
}
if var_2267, ok := var_2263.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath); ok {
_ = var_2267

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2263))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((var_2267))
if true {
var var_2268 Tup_
var_2268 = Tup_{}
_ = var_2268
var_2268 = Tup_{}
return var_2268
goto var_2264

}
}

                    var_2264:
                    if false {}

var_2269 = var_2262
}
return var_2269
return Tup_{}
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) is_none() bool {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2270 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2270
var_2270 = IDENTITY((self))
var var_2271 Tag__none
_ = var_2271
var_2271 = Tag__none{}
var var_2272 bool
_ = var_2272
var_2272 = func() bool {
                    var p1 any = ((*var_2270).inner)
                    var p2 any = (var_2271)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath:
                            switch p2.(type) {
                                case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath:
                                return true
                            }
                        }

                        switch p1.(type) {
                        case Tag__none:
                            switch p2.(type) {
                                case Tag__none:
                                return true
                            }
                        }

return false }()
return var_2272
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) filter(predicate func(value *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) bool) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = predicate
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2273 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2273
var_2273 = IDENTITY((self))
var var_2274 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2274

var var_2275 any
_ = var_2275
var_2275 = (*var_2273).inner
if var_2277, ok := var_2275.(Tag__none); ok {
_ = var_2277

if true {
var var_2278 Tag__none
_ = var_2278
var_2278 = Tag__none{}
var var_2279 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2279
var_2279 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{inner: var_2278}
var_2274 = var_2279
goto var_2276

}
}
if var_2280, ok := var_2275.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath); ok {
_ = var_2280

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2275))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((var_2280))
if true {
var var_2290 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2290
{
var var_2281 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2281
var_2281 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((some))
var var_2282 bool
_ = var_2282
var_2282 = predicate(var_2281.value)
var var_2283 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2283
if var_2282 {
var var_2286 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2286
{
var var_2284 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2284
var_2284 = IDENTITY((self))
var var_2285 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2285
var_2285 = *var_2284
var_2286 = var_2285
}
var_2283 = var_2286
} else {
var var_2289 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2289
{
var var_2287 Tag__none
_ = var_2287
var_2287 = Tag__none{}
var var_2288 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2288
var_2288 = &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{inner: var_2287}
var_2289 = var_2288
}
var_2283 = var_2289
}
var_2290 = var_2283
}
var_2274 = var_2290
goto var_2276

}
}

                    var_2276:
                    if false {}

return var_2274
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) or_else(other *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = other
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2291 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2291
var_2291 = IDENTITY((self))
var var_2292 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2292

var var_2293 any
_ = var_2293
var_2293 = (*var_2291).inner
if var_2295, ok := var_2293.(Tag__none); ok {
_ = var_2295

if true {
var var_2296 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2296
var_2296 = Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((other))
var_2292 = var_2296
goto var_2294

}
}
if var_2297, ok := var_2293.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath); ok {
_ = var_2297

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2293))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((var_2297))
if true {
var var_2298 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2298
var_2298 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((some))
var_2292 = var_2298.value
goto var_2294

}
}

                    var_2294:
                    if false {}

return var_2292
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) or_else_get(other_supplier func() *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = other_supplier
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2299 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2299
var_2299 = IDENTITY((self))
var var_2300 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2300

var var_2301 any
_ = var_2301
var_2301 = (*var_2299).inner
if var_2303, ok := var_2301.(Tag__none); ok {
_ = var_2303

if true {
var var_2304 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2304
var_2304 = other_supplier()
var_2300 = var_2304
goto var_2302

}
}
if var_2305, ok := var_2301.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath); ok {
_ = var_2305

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2301))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((var_2305))
if true {
var var_2306 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2306
var_2306 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((some))
var_2300 = var_2306.value
goto var_2302

}
}

                    var_2302:
                    if false {}

return var_2300
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) unwrap() *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2307 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2307
var_2307 = IDENTITY((self))
var var_2308 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2308

var var_2309 any
_ = var_2309
var_2309 = (*var_2307).inner
if var_2311, ok := var_2309.(Tag__none); ok {
_ = var_2311

if true {
var var_2312 string
_ = var_2312
var_2312 = "unwrap on none"
std_____error_____panic(var_2312)
goto var_2310

}
}
if var_2313, ok := var_2309.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath); ok {
_ = var_2313

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2309))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((var_2313))
if true {
var var_2314 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2314
var_2314 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((some))
var_2308 = var_2314.value
goto var_2310

}
}

                    var_2310:
                    if false {}

return var_2308
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) expect(msg string) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = msg
var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2315 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2315
var_2315 = IDENTITY((self))
var var_2316 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2316

var var_2317 any
_ = var_2317
var_2317 = (*var_2315).inner
if var_2319, ok := var_2317.(Tag__none); ok {
_ = var_2319

if true {
var var_2321 string
_ = var_2321
{
var var_2320 string
_ = var_2320
var_2320 = IDENTITY((msg))
var_2321 = var_2320
}
var var_2322 string
_ = var_2322
var_2322 = "expect failed: " + var_2321
std_____error_____panic(var_2322)
goto var_2318

}
}
if var_2323, ok := var_2317.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath); ok {
_ = var_2323

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2317))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((var_2323))
if true {
var var_2324 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2324
var_2324 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((some))
var_2316 = var_2324.value
goto var_2318

}
}

                    var_2318:
                    if false {}

return var_2316
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) iter() *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var self **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2340 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2340
{
var var_2325 bool
_ = var_2325
var_2325 = false
var consumed bool
_ = consumed
consumed = var_2325
var var_2326 **Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2326
var_2326 = IDENTITY((self))
var var_2327 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2327

var var_2328 any
_ = var_2328
var_2328 = (*var_2326).inner
if var_2330, ok := var_2328.(Tag__none); ok {
_ = var_2330

if true {
var var_2331 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2331
var_2331 = std_____col_____Iter_____empty_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath()
var_2327 = var_2331
goto var_2329

}
}
if var_2332, ok := var_2328.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath); ok {
_ = var_2332

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2328))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((var_2332))
if true {
var var_2333 func() any
_ = var_2333
var_2333 = func() any {
var var_2334 bool
_ = var_2334
var_2334 = IDENTITY((consumed))
var var_2335 any
_ = var_2335
if var_2334 {
var var_2336 Tag__no_next_elem
_ = var_2336
var_2336 = Tag__no_next_elem{}
return var_2336
} else {
var var_2337 bool
_ = var_2337
var_2337 = true
consumed = var_2337
var var_2338 *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2338
var_2338 = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((some))
return var_2338.value
}
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_2339 *Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2339
var_2339 = std_____col_____Iter_____from_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath(var_2333)
var_2327 = var_2339
goto var_2329

}
}

                    var_2329:
                    if false {}

var_2340 = var_2327
}
return var_2340
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy(self *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{
inner: func() any {
                    var p1 any = (self.inner)

                        switch p1.(type) {
                        case *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath:
                            tmp := p1.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath)
                            _ = tmp
                            return Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }(),
}

}
type Struct_std_____col_____Iter_____Tup_int_Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____col_____Iter_____Struct_std_____path_____FilePath struct {
next_fn func() any
}



func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) rev() *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2358 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2358
{
var stack *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = stack
stack = std_____col_____ArrayList_____new_____Struct_std_____path_____FilePath()
var var_2341 func() any
_ = var_2341
var_2341 = func() any {
if false {goto label_2343}
label_2343:
for {
var var_2342 bool
_ = var_2342
var_2342 = true
if var_2342 {
var var_2352 **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_2352
{
var var_2344 any
_ = var_2344
var_2344 = (*self).next_fn()
var var_2345 **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_2345

var var_2346 any
_ = var_2346
var_2346 = var_2344
if var_2348, ok := var_2346.(*Struct_std_____path_____FilePath); ok {
_ = var_2348

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2346))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2348))
if true {
var var_2349 *Struct_std_____path_____FilePath
_ = var_2349
var_2349 = Struct_std_____path_____FilePath_Copy((t))
var var_2350 **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_2350
var_2350 = stack.push(var_2349)
var_2345 = var_2350
goto var_2347

}
}
if var_2351, ok := var_2346.(Tag__no_next_elem); ok {
_ = var_2351

if true {
break label_2343
goto var_2347

}
}

                    var_2347:
                    if false {}

var_2352 = var_2345
}
} else {
break label_2343
}
}
var maybe_next *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = maybe_next
maybe_next = stack.pop()
var var_2353 bool
_ = var_2353
var_2353 = maybe_next.is_some()
var var_2354 any
_ = var_2354
if var_2353 {
var var_2355 *Struct_std_____path_____FilePath
_ = var_2355
var_2355 = maybe_next.unwrap()
return var_2355
} else {
var var_2356 Tag__no_next_elem
_ = var_2356
var_2356 = Tag__no_next_elem{}
return var_2356
}
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var var_2357 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2357
var_2357 = std_____col_____Iter_____from_____Struct_std_____path_____FilePath(var_2341)
var_2358 = var_2357
}
return var_2358
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) next() any {

var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2359 any
_ = var_2359
var_2359 = (*self).next_fn()
var var_2360 any
_ = var_2360

var var_2361 any
_ = var_2361
var_2361 = var_2359
if var_2363, ok := var_2361.(*Struct_std_____path_____FilePath); ok {
_ = var_2363

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2361))
                            _ = iface_ptr

var x *Struct_std_____path_____FilePath
_ = x
x = Struct_std_____path_____FilePath_Copy((var_2363))
if true {
var var_2364 *Struct_std_____path_____FilePath
_ = var_2364
var_2364 = Struct_std_____path_____FilePath_Copy((x))
var_2360 = var_2364
goto var_2362

}
}
if var_2365, ok := var_2361.(Tag__no_next_elem); ok {
_ = var_2365

if true {
var var_2366 Tag__no_next_elem
_ = var_2366
var_2366 = Tag__no_next_elem{}
var_2360 = var_2366
goto var_2362

}
}

                    var_2362:
                    if false {}

return var_2360
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) for_each(f func(*Struct_std_____path_____FilePath) Tup_) Tup_ {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2379 Tup_
var_2379 = Tup_{}
_ = var_2379
{
if false {goto label_2368}
label_2368:
for {
var var_2367 bool
_ = var_2367
var_2367 = true
if var_2367 {
var var_2378 Tup_
var_2378 = Tup_{}
_ = var_2378
{
var next any
_ = next
next = (*self).next_fn()
var var_2369 any
_ = var_2369
var_2369 = func() any {
                    var p1 any = (next)

                        switch p1.(type) {
                        case *Struct_std_____path_____FilePath:
                            tmp := p1.(*Struct_std_____path_____FilePath)
                            _ = tmp
                            return Struct_std_____path_____FilePath_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_2370 Tup_
var_2370 = Tup_{}
_ = var_2370

var var_2371 any
_ = var_2371
var_2371 = var_2369
if var_2373, ok := var_2371.(*Struct_std_____path_____FilePath); ok {
_ = var_2373

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2371))
                            _ = iface_ptr

var str *Struct_std_____path_____FilePath
_ = str
str = Struct_std_____path_____FilePath_Copy((var_2373))
if true {
var var_2374 *Struct_std_____path_____FilePath
_ = var_2374
var_2374 = Struct_std_____path_____FilePath_Copy((str))
var var_2375 Tup_
var_2375 = Tup_{}
_ = var_2375
var_2375 = f(var_2374)
var_2370 = var_2375
goto var_2372

}
}
if var_2376, ok := var_2371.(Tag__no_next_elem); ok {
_ = var_2376

if true {
var var_2377 Tup_
var_2377 = Tup_{}
_ = var_2377
var_2377 = Tup_{}
return var_2377
goto var_2372

}
}

                    var_2372:
                    if false {}

var_2378 = var_2370
}
} else {
break label_2368
}
}
var_2379 = Tup_{}
}
return var_2379
return Tup_{}
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) chain(other_iters any) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = other_iters
var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2380 any
_ = var_2380
var_2380 = func() any {
                    var p1 any = (other_iters)

                        switch p1.(type) {
                        case []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath:
                            tmp := p1.([]*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath)
                            _ = tmp
                            return Array_Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((tmp))
                        }

                        switch p1.(type) {
                        case *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath:
                            tmp := p1.(*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath)
                            _ = tmp
                            return Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_2381 []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2381

var var_2382 any
_ = var_2382
var_2382 = var_2380
if var_2384, ok := var_2382.([]*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath); ok {
_ = var_2384

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2382))
                            _ = iface_ptr

var iters []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = iters
iters = Array_Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((var_2384))
if true {
var var_2385 []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2385
var_2385 = Array_Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((iters))
var_2381 = var_2385
goto var_2383

}
}
if var_2386, ok := var_2382.(*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath); ok {
_ = var_2386

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2382))
                            _ = iface_ptr

var iter *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = iter
iter = Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((var_2386))
if true {
var var_2387 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2387
var_2387 = Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((iter))
var var_2388 []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2388
var_2388 = []*Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{var_2387}
var_2381 = var_2388
goto var_2383

}
}

                    var_2383:
                    if false {}

var remaining_iters *Struct_std_____col_____ArrayList_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = remaining_iters
remaining_iters = std_____col_____ArrayList_____from_array_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath(var_2381)
var var_2389 **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2389
var_2389 = IDENTITY((self))
var current_iter **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = current_iter
current_iter = var_2389
var var_2390 func() any
_ = var_2390
var_2390 = func() any {
if false {goto label_2392}
label_2392:
for {
var var_2391 bool
_ = var_2391
var_2391 = true
if var_2391 {
var next_elem any
_ = next_elem
next_elem = (*current_iter).next_fn()
var var_2393 any
_ = var_2393
var_2393 = func() any {
                    var p1 any = (next_elem)

                        switch p1.(type) {
                        case *Struct_std_____path_____FilePath:
                            tmp := p1.(*Struct_std_____path_____FilePath)
                            _ = tmp
                            return Struct_std_____path_____FilePath_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_2394 Tup_
var_2394 = Tup_{}
_ = var_2394

var var_2395 any
_ = var_2395
var_2395 = var_2393
if var_2397, ok := var_2395.(*Struct_std_____path_____FilePath); ok {
_ = var_2397

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2395))
                            _ = iface_ptr

var value *Struct_std_____path_____FilePath
_ = value
value = Struct_std_____path_____FilePath_Copy((var_2397))
if true {
var var_2398 *Struct_std_____path_____FilePath
_ = var_2398
var_2398 = Struct_std_____path_____FilePath_Copy((value))
return var_2398
goto var_2396

}
}
if var_2399, ok := var_2395.(Tag__no_next_elem); ok {
_ = var_2399

if true {
var var_2400 Tup_
var_2400 = Tup_{}
_ = var_2400
{
var_2400 = Tup_{}
}
var_2394 = var_2400
goto var_2396

}
}

                    var_2396:
                    if false {}

var var_2401 int
_ = var_2401
var_2401 = remaining_iters.len()
var var_2402 int
_ = var_2402
var_2402 = 0
var var_2403 bool
_ = var_2403
switch Int_Ord((var_2401), &(var_2402)).(type) {
                            case Tag__greater:
                            var_2403 = true
                            default:
                            var_2403 = false
                            }

var var_2404 Tup_
var_2404 = Tup_{}
_ = var_2404
if var_2403 {
var next_iter *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = next_iter
next_iter = remaining_iters.pop()
var var_2405 *Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2405
var_2405 = Struct_std_____opt_____Opt_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((next_iter))
var var_2406 any
_ = var_2406

var var_2407 any
_ = var_2407
var_2407 = var_2405.inner
if var_2409, ok := var_2407.(*Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath); ok {
_ = var_2409

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2407))
                            _ = iface_ptr

var some *Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = some
some = Struct_std_____opt_____Some_____Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy((var_2409))
if true {
var var_2410 **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2410
var_2410 = &some.value
current_iter = var_2410
continue label_2392
goto var_2408

}
}
if var_2411, ok := var_2407.(Tag__none); ok {
_ = var_2411

if true {
var var_2412 Tag__no_next_elem
_ = var_2412
var_2412 = Tag__no_next_elem{}
return var_2412
goto var_2408

}
}

                    var_2408:
                    if false {}

}
var var_2413 Tag__no_next_elem
_ = var_2413
var_2413 = Tag__no_next_elem{}
return var_2413
} else {
break label_2392
}
}
var var_2414 Tag__no_next_elem
_ = var_2414
var_2414 = Tag__no_next_elem{}
return var_2414
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_2390
var var_2415 func() any
_ = var_2415
var_2415 = IDENTITY((next_fn))
var var_2416 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2416
var_2416 = std_____col_____Iter_____from_____Struct_std_____path_____FilePath(var_2415)
return var_2416
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) skip_while(predicate func(*Struct_std_____path_____FilePath) bool) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = predicate
var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2434 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2434
{
var var_2417 func() any
_ = var_2417
var_2417 = func() any {
if false {goto label_2419}
label_2419:
for {
var var_2418 bool
_ = var_2418
var_2418 = true
if var_2418 {
var var_2420 any
_ = var_2420
var_2420 = (*self).next_fn()
var var_2421 any
_ = var_2421

var var_2422 any
_ = var_2422
var_2422 = var_2420
if var_2424, ok := var_2422.(*Struct_std_____path_____FilePath); ok {
_ = var_2424

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2422))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2424))
if true {
var var_2425 *Struct_std_____path_____FilePath
_ = var_2425
var_2425 = Struct_std_____path_____FilePath_Copy((t))
var var_2426 bool
_ = var_2426
var_2426 = predicate(var_2425)
var var_2427 any
_ = var_2427
if var_2426 {
continue label_2419
} else {
var var_2428 *Struct_std_____path_____FilePath
_ = var_2428
var_2428 = Struct_std_____path_____FilePath_Copy((t))
return var_2428
}
var_2421 = var_2427
goto var_2423

}
}
if var_2429, ok := var_2422.(Tag__no_next_elem); ok {
_ = var_2429

if true {
var var_2430 Tag__no_next_elem
_ = var_2430
var_2430 = Tag__no_next_elem{}
return var_2430
goto var_2423

}
}

                    var_2423:
                    if false {}

} else {
break label_2419
}
}
var var_2431 string
_ = var_2431
var_2431 = "skip_while"
std_____error_____unreachable(var_2431)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_2417
var var_2432 func() any
_ = var_2432
var_2432 = IDENTITY((next_fn))
var var_2433 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2433
var_2433 = std_____col_____Iter_____from_____Struct_std_____path_____FilePath(var_2432)
var_2434 = var_2433
}
return var_2434
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) take_while(predicate func(*Struct_std_____path_____FilePath) bool) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = predicate
var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2453 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2453
{
var var_2435 func() any
_ = var_2435
var_2435 = func() any {
if false {goto label_2437}
label_2437:
for {
var var_2436 bool
_ = var_2436
var_2436 = true
if var_2436 {
var var_2438 any
_ = var_2438
var_2438 = (*self).next_fn()
var var_2439 any
_ = var_2439

var var_2440 any
_ = var_2440
var_2440 = var_2438
if var_2442, ok := var_2440.(*Struct_std_____path_____FilePath); ok {
_ = var_2442

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2440))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2442))
if true {
var var_2443 *Struct_std_____path_____FilePath
_ = var_2443
var_2443 = Struct_std_____path_____FilePath_Copy((t))
var var_2444 bool
_ = var_2444
var_2444 = predicate(var_2443)
var var_2445 any
_ = var_2445
if var_2444 {
var var_2446 *Struct_std_____path_____FilePath
_ = var_2446
var_2446 = Struct_std_____path_____FilePath_Copy((t))
return var_2446
} else {
var var_2447 Tag__no_next_elem
_ = var_2447
var_2447 = Tag__no_next_elem{}
return var_2447
}
var_2439 = var_2445
goto var_2441

}
}
if var_2448, ok := var_2440.(Tag__no_next_elem); ok {
_ = var_2448

if true {
var var_2449 Tag__no_next_elem
_ = var_2449
var_2449 = Tag__no_next_elem{}
return var_2449
goto var_2441

}
}

                    var_2441:
                    if false {}

} else {
break label_2437
}
}
var var_2450 string
_ = var_2450
var_2450 = "take_while"
std_____error_____unreachable(var_2450)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_2435
var var_2451 func() any
_ = var_2451
var_2451 = IDENTITY((next_fn))
var var_2452 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2452
var_2452 = std_____col_____Iter_____from_____Struct_std_____path_____FilePath(var_2451)
var_2453 = var_2452
}
return var_2453
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) filter(predicate func(*Struct_std_____path_____FilePath) bool) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = predicate
var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2471 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2471
{
var var_2454 func() any
_ = var_2454
var_2454 = func() any {
if false {goto label_2456}
label_2456:
for {
var var_2455 bool
_ = var_2455
var_2455 = true
if var_2455 {
var var_2457 any
_ = var_2457
var_2457 = (*self).next_fn()
var var_2458 any
_ = var_2458

var var_2459 any
_ = var_2459
var_2459 = var_2457
if var_2461, ok := var_2459.(*Struct_std_____path_____FilePath); ok {
_ = var_2461

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2459))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2461))
if true {
var var_2462 *Struct_std_____path_____FilePath
_ = var_2462
var_2462 = Struct_std_____path_____FilePath_Copy((t))
var var_2463 bool
_ = var_2463
var_2463 = predicate(var_2462)
var var_2464 any
_ = var_2464
if var_2463 {
var var_2465 *Struct_std_____path_____FilePath
_ = var_2465
var_2465 = Struct_std_____path_____FilePath_Copy((t))
return var_2465
} else {
continue label_2456
}
goto var_2460

}
}
if var_2466, ok := var_2459.(Tag__no_next_elem); ok {
_ = var_2466

if true {
var var_2467 Tag__no_next_elem
_ = var_2467
var_2467 = Tag__no_next_elem{}
return var_2467
goto var_2460

}
}

                    var_2460:
                    if false {}

} else {
break label_2456
}
}
var var_2468 string
_ = var_2468
var_2468 = "filter"
std_____error_____unreachable(var_2468)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_2454
var var_2469 func() any
_ = var_2469
var_2469 = IDENTITY((next_fn))
var var_2470 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2470
var_2470 = std_____col_____Iter_____from_____Struct_std_____path_____FilePath(var_2469)
var_2471 = var_2470
}
return var_2471
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) skip(amount int) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = amount
var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2494 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2494
{
var var_2472 int
_ = var_2472
var_2472 = IDENTITY((amount))
var to_skip int
_ = to_skip
to_skip = var_2472
var var_2473 func() any
_ = var_2473
var_2473 = func() any {
if false {goto label_2475}
label_2475:
for {
var var_2474 bool
_ = var_2474
var_2474 = true
if var_2474 {
var next any
_ = next
next = (*self).next_fn()
var var_2476 int
_ = var_2476
var_2476 = IDENTITY((to_skip))
var var_2477 int
_ = var_2477
var_2477 = 0
var var_2478 bool
_ = var_2478
switch Int_Ord((var_2476), &(var_2477)).(type) {
                            case Tag__greater:
                            var_2478 = true
                            default:
                            var_2478 = false
                            }

var var_2479 Tup_
var_2479 = Tup_{}
_ = var_2479
if var_2478 {
var var_2480 int
_ = var_2480
var_2480 = IDENTITY((to_skip))
var var_2481 int
_ = var_2481
var_2481 = 1
var var_2482 int
_ = var_2482
var_2482 = var_2480 - var_2481
to_skip = var_2482
continue label_2475
}
var var_2483 any
_ = var_2483
var_2483 = func() any {
                    var p1 any = (next)

                        switch p1.(type) {
                        case *Struct_std_____path_____FilePath:
                            tmp := p1.(*Struct_std_____path_____FilePath)
                            _ = tmp
                            return Struct_std_____path_____FilePath_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_2484 *Struct_std_____path_____FilePath
_ = var_2484

var var_2485 any
_ = var_2485
var_2485 = var_2483
if var_2487, ok := var_2485.(*Struct_std_____path_____FilePath); ok {
_ = var_2487

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2485))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2487))
if true {
var var_2488 *Struct_std_____path_____FilePath
_ = var_2488
var_2488 = Struct_std_____path_____FilePath_Copy((t))
var_2484 = var_2488
goto var_2486

}
}
if var_2489, ok := var_2485.(Tag__no_next_elem); ok {
_ = var_2489

if true {
var var_2490 Tag__no_next_elem
_ = var_2490
var_2490 = Tag__no_next_elem{}
return var_2490
goto var_2486

}
}

                    var_2486:
                    if false {}

return var_2484
} else {
break label_2475
}
}
var var_2491 string
_ = var_2491
var_2491 = "skip"
std_____error_____unreachable(var_2491)
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_2473
var var_2492 func() any
_ = var_2492
var_2492 = IDENTITY((next_fn))
var var_2493 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2493
var_2493 = std_____col_____Iter_____from_____Struct_std_____path_____FilePath(var_2492)
var_2494 = var_2493
}
return var_2494
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) take(amount int) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = amount
var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2515 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2515
{
var var_2495 int
_ = var_2495
var_2495 = 0
var to_take int
_ = to_take
to_take = var_2495
var var_2496 func() any
_ = var_2496
var_2496 = func() any {
var next any
_ = next
next = (*self).next_fn()
var var_2497 int
_ = var_2497
var_2497 = IDENTITY((to_take))
var var_2498 int
_ = var_2498
var_2498 = IDENTITY((amount))
var var_2499 bool
_ = var_2499
switch Int_Ord((var_2497), &(var_2498)).(type) {
                            case Tag__greater:
                            var_2499 = true
                            case Tag__equal:
                            var_2499 = true
                            default:
                            var_2499 = false
                            }

var var_2500 Tup_
var_2500 = Tup_{}
_ = var_2500
if var_2499 {
var var_2501 Tag__no_next_elem
_ = var_2501
var_2501 = Tag__no_next_elem{}
return var_2501
}
var var_2502 int
_ = var_2502
var_2502 = IDENTITY((to_take))
var var_2503 int
_ = var_2503
var_2503 = 1
var var_2504 int
_ = var_2504
var_2504 = var_2502 + var_2503
to_take = var_2504
var var_2505 any
_ = var_2505
var_2505 = func() any {
                    var p1 any = (next)

                        switch p1.(type) {
                        case *Struct_std_____path_____FilePath:
                            tmp := p1.(*Struct_std_____path_____FilePath)
                            _ = tmp
                            return Struct_std_____path_____FilePath_Copy((tmp))
                        }

                        switch p1.(type) {
                        case Tag__no_next_elem:
                            tmp := p1.(Tag__no_next_elem)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
var var_2506 *Struct_std_____path_____FilePath
_ = var_2506

var var_2507 any
_ = var_2507
var_2507 = var_2505
if var_2509, ok := var_2507.(*Struct_std_____path_____FilePath); ok {
_ = var_2509

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2507))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2509))
if true {
var var_2510 *Struct_std_____path_____FilePath
_ = var_2510
var_2510 = Struct_std_____path_____FilePath_Copy((t))
var_2506 = var_2510
goto var_2508

}
}
if var_2511, ok := var_2507.(Tag__no_next_elem); ok {
_ = var_2511

if true {
var var_2512 Tag__no_next_elem
_ = var_2512
var_2512 = Tag__no_next_elem{}
return var_2512
goto var_2508

}
}

                    var_2508:
                    if false {}

return var_2506
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_2496
var var_2513 func() any
_ = var_2513
var_2513 = IDENTITY((next_fn))
var var_2514 *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = var_2514
var_2514 = std_____col_____Iter_____from_____Struct_std_____path_____FilePath(var_2513)
var_2515 = var_2514
}
return var_2515
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) indexed() *Struct_std_____col_____Iter_____Tup_int_Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2516 int
_ = var_2516
var_2516 = 0
var index int
_ = index
index = var_2516
var var_2517 func() any
_ = var_2517
var_2517 = func() any {
var var_2518 any
_ = var_2518
var_2518 = (*self).next_fn()
var var_2519 *Struct_std_____path_____FilePath
_ = var_2519

var var_2520 any
_ = var_2520
var_2520 = var_2518
if var_2522, ok := var_2520.(*Struct_std_____path_____FilePath); ok {
_ = var_2522

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2520))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2522))
if true {
var var_2523 *Struct_std_____path_____FilePath
_ = var_2523
var_2523 = Struct_std_____path_____FilePath_Copy((t))
var_2519 = var_2523
goto var_2521

}
}
if var_2524, ok := var_2520.(Tag__no_next_elem); ok {
_ = var_2524

if true {
var var_2525 Tag__no_next_elem
_ = var_2525
var_2525 = Tag__no_next_elem{}
return var_2525
goto var_2521

}
}

                    var_2521:
                    if false {}

var next *Struct_std_____path_____FilePath
_ = next
next = var_2519
var var_2526 int
_ = var_2526
var_2526 = IDENTITY((index))
var index_to_return int
_ = index_to_return
index_to_return = var_2526
var var_2527 int
_ = var_2527
var_2527 = IDENTITY((index))
var var_2528 int
_ = var_2528
var_2528 = 1
var var_2529 int
_ = var_2529
var_2529 = var_2527 + var_2528
index = var_2529
var var_2530 int
_ = var_2530
var_2530 = IDENTITY((index_to_return))
var var_2531 *Struct_std_____path_____FilePath
_ = var_2531
var_2531 = Struct_std_____path_____FilePath_Copy((next))
var var_2532 Tup_int_Struct_std_____path_____FilePath
_ = var_2532
var_2532 = Tup_int_Struct_std_____path_____FilePath{var_2530, var_2531}
return var_2532
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var next_fn func() any
_ = next_fn
next_fn = var_2517
var var_2533 func() any
_ = var_2533
var_2533 = IDENTITY((next_fn))
var var_2534 *Struct_std_____col_____Iter_____Tup_int_Struct_std_____path_____FilePath
_ = var_2534
var_2534 = std_____col_____Iter_____from_____Tup_int_Struct_std_____path_____FilePath(var_2533)
return var_2534
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Tup_int_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) reduce(f func(acc **Struct_std_____path_____FilePath,elem *Struct_std_____path_____FilePath) Tup_) *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2535 any
_ = var_2535
var_2535 = (*self).next()
var var_2536 *Struct_std_____path_____FilePath
_ = var_2536

var var_2537 any
_ = var_2537
var_2537 = var_2535
if var_2539, ok := var_2537.(*Struct_std_____path_____FilePath); ok {
_ = var_2539

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2537))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2539))
if true {
var var_2540 *Struct_std_____path_____FilePath
_ = var_2540
var_2540 = Struct_std_____path_____FilePath_Copy((t))
var_2536 = var_2540
goto var_2538

}
}
if var_2541, ok := var_2537.(Tag__no_next_elem); ok {
_ = var_2541

if true {
var var_2542 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2542
var_2542 = std_____opt_____Opt_____none_____Struct_std_____path_____FilePath()
return var_2542
goto var_2538

}
}

                    var_2538:
                    if false {}

var first *Struct_std_____path_____FilePath
_ = first
first = var_2536
if false {goto label_2544}
label_2544:
for {
var var_2543 bool
_ = var_2543
var_2543 = true
if var_2543 {
var var_2556 Tup_
var_2556 = Tup_{}
_ = var_2556
{
var var_2545 any
_ = var_2545
var_2545 = (*self).next()
var var_2546 *Struct_std_____path_____FilePath
_ = var_2546

var var_2547 any
_ = var_2547
var_2547 = var_2545
if var_2549, ok := var_2547.(*Struct_std_____path_____FilePath); ok {
_ = var_2549

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2547))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2549))
if true {
var var_2550 *Struct_std_____path_____FilePath
_ = var_2550
var_2550 = Struct_std_____path_____FilePath_Copy((t))
var_2546 = var_2550
goto var_2548

}
}
if var_2551, ok := var_2547.(Tag__no_next_elem); ok {
_ = var_2551

if true {
break label_2544
goto var_2548

}
}

                    var_2548:
                    if false {}

var elem *Struct_std_____path_____FilePath
_ = elem
elem = var_2546
var var_2552 **Struct_std_____path_____FilePath
_ = var_2552
var_2552 = &first
var var_2553 *Struct_std_____path_____FilePath
_ = var_2553
var_2553 = Struct_std_____path_____FilePath_Copy((elem))
var var_2554 Tup_
var_2554 = Tup_{}
_ = var_2554
var_2554 = f(var_2552, var_2553)
var var_2555 Tup_
var_2555 = Tup_{}
_ = var_2555
var_2555 = Tup_{}
var_2556 = var_2555
}
} else {
break label_2544
}
}
var var_2557 *Struct_std_____path_____FilePath
_ = var_2557
var_2557 = Struct_std_____path_____FilePath_Copy((first))
var var_2558 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2558
var_2558 = std_____opt_____Opt_____some_____Struct_std_____path_____FilePath(var_2557)
return var_2558
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) first() *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2559 any
_ = var_2559
var_2559 = (*self).next_fn()
var var_2560 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2560

var var_2561 any
_ = var_2561
var_2561 = var_2559
if var_2563, ok := var_2561.(*Struct_std_____path_____FilePath); ok {
_ = var_2563

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2561))
                            _ = iface_ptr

var value *Struct_std_____path_____FilePath
_ = value
value = Struct_std_____path_____FilePath_Copy((var_2563))
if true {
var var_2564 *Struct_std_____path_____FilePath
_ = var_2564
var_2564 = Struct_std_____path_____FilePath_Copy((value))
var var_2565 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2565
var_2565 = std_____opt_____Opt_____some_____Struct_std_____path_____FilePath(var_2564)
var_2560 = var_2565
goto var_2562

}
}
if var_2566, ok := var_2561.(Tag__no_next_elem); ok {
_ = var_2566

if true {
var var_2567 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2567
var_2567 = std_____opt_____Opt_____none_____Struct_std_____path_____FilePath()
var_2560 = var_2567
goto var_2562

}
}

                    var_2562:
                    if false {}

return var_2560
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) into_list() *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var arr *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = arr
arr = std_____col_____ArrayList_____new_____Struct_std_____path_____FilePath()
if false {goto label_2569}
label_2569:
for {
var var_2568 bool
_ = var_2568
var_2568 = true
if var_2568 {
var var_2579 **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_2579
{
var var_2570 any
_ = var_2570
var_2570 = (*self).next_fn()
var var_2571 **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_2571

var var_2572 any
_ = var_2572
var_2572 = var_2570
if var_2574, ok := var_2572.(*Struct_std_____path_____FilePath); ok {
_ = var_2574

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2572))
                            _ = iface_ptr

var elem *Struct_std_____path_____FilePath
_ = elem
elem = Struct_std_____path_____FilePath_Copy((var_2574))
if true {
var var_2575 *Struct_std_____path_____FilePath
_ = var_2575
var_2575 = Struct_std_____path_____FilePath_Copy((elem))
var var_2576 **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_2576
var_2576 = arr.push(var_2575)
var_2571 = var_2576
goto var_2573

}
}
if var_2577, ok := var_2572.(Tag__no_next_elem); ok {
_ = var_2577

if true {
var var_2578 *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_2578
var_2578 = Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath_Copy((arr))
return var_2578
goto var_2573

}
}

                    var_2573:
                    if false {}

var_2579 = var_2571
}
} else {
break label_2569
}
}
var var_2580 *Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
_ = var_2580
var_2580 = Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath_Copy((arr))
return var_2580
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) count() uint {

var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2599 uint
_ = var_2599
{
var var_2581 uint
_ = var_2581
var_2581 = 0
var var_2582 uint
_ = var_2582
var_2582 = uint(var_2581)
var result uint
_ = result
result = var_2582
if false {goto label_2584}
label_2584:
for {
var var_2583 bool
_ = var_2583
var_2583 = true
if var_2583 {
var var_2597 Tup_
var_2597 = Tup_{}
_ = var_2597
{
var var_2585 any
_ = var_2585
var_2585 = (*self).next_fn()
var var_2586 Tup_
var_2586 = Tup_{}
_ = var_2586

var var_2587 any
_ = var_2587
var_2587 = var_2585
if var_2589, ok := var_2587.(*Struct_std_____path_____FilePath); ok {
_ = var_2589

if true {
var var_2595 Tup_
var_2595 = Tup_{}
_ = var_2595
{
var var_2590 uint
_ = var_2590
var_2590 = IDENTITY((result))
var var_2591 uint
_ = var_2591
var_2591 = 1
var var_2592 uint
_ = var_2592
var_2592 = uint(var_2591)
var var_2593 uint
_ = var_2593
var_2593 = var_2590 + var_2592
result = var_2593
var var_2594 Tup_
var_2594 = Tup_{}
_ = var_2594
var_2594 = Tup_{}
var_2595 = var_2594
}
var_2586 = var_2595
goto var_2588

}
}
if var_2596, ok := var_2587.(Tag__no_next_elem); ok {
_ = var_2596

if true {
break label_2584
goto var_2588

}
}

                    var_2588:
                    if false {}

var_2597 = var_2586
}
} else {
break label_2584
}
}
var var_2598 uint
_ = var_2598
var_2598 = IDENTITY((result))
var_2599 = var_2598
}
return var_2599
var ΔΔΔretΔΔΔ *uint
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) find(f func(e *Struct_std_____path_____FilePath) bool) *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
if false {goto label_2601}
label_2601:
for {
var var_2600 bool
_ = var_2600
var_2600 = true
if var_2600 {
var var_2614 Tup_
var_2614 = Tup_{}
_ = var_2614
{
var var_2602 any
_ = var_2602
var_2602 = (*self).next_fn()
var var_2603 Tup_
var_2603 = Tup_{}
_ = var_2603

var var_2604 any
_ = var_2604
var_2604 = var_2602
if var_2606, ok := var_2604.(*Struct_std_____path_____FilePath); ok {
_ = var_2606

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2604))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2606))
if true {
var var_2612 Tup_
var_2612 = Tup_{}
_ = var_2612
{
var var_2607 *Struct_std_____path_____FilePath
_ = var_2607
var_2607 = Struct_std_____path_____FilePath_Copy((t))
var var_2608 bool
_ = var_2608
var_2608 = f(var_2607)
var var_2609 Tup_
var_2609 = Tup_{}
_ = var_2609
if var_2608 {
var var_2610 *Struct_std_____path_____FilePath
_ = var_2610
var_2610 = Struct_std_____path_____FilePath_Copy((t))
var var_2611 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2611
var_2611 = std_____opt_____Opt_____some_____Struct_std_____path_____FilePath(var_2610)
return var_2611
}
var_2612 = var_2609
}
var_2603 = var_2612
goto var_2605

}
}
if var_2613, ok := var_2604.(Tag__no_next_elem); ok {
_ = var_2613

if true {
break label_2601
goto var_2605

}
}

                    var_2605:
                    if false {}

var_2614 = var_2603
}
} else {
break label_2601
}
}
var var_2615 *Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
_ = var_2615
var_2615 = std_____opt_____Opt_____none_____Struct_std_____path_____FilePath()
return var_2615
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) any(f func(e *Struct_std_____path_____FilePath) bool) bool {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2631 bool
_ = var_2631
{
if false {goto label_2617}
label_2617:
for {
var var_2616 bool
_ = var_2616
var_2616 = true
if var_2616 {
var var_2629 Tup_
var_2629 = Tup_{}
_ = var_2629
{
var var_2618 any
_ = var_2618
var_2618 = (*self).next_fn()
var var_2619 Tup_
var_2619 = Tup_{}
_ = var_2619

var var_2620 any
_ = var_2620
var_2620 = var_2618
if var_2622, ok := var_2620.(*Struct_std_____path_____FilePath); ok {
_ = var_2622

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2620))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2622))
if true {
var var_2627 Tup_
var_2627 = Tup_{}
_ = var_2627
{
var var_2623 *Struct_std_____path_____FilePath
_ = var_2623
var_2623 = Struct_std_____path_____FilePath_Copy((t))
var var_2624 bool
_ = var_2624
var_2624 = f(var_2623)
var var_2625 Tup_
var_2625 = Tup_{}
_ = var_2625
if var_2624 {
var var_2626 bool
_ = var_2626
var_2626 = true
return var_2626
}
var_2627 = var_2625
}
var_2619 = var_2627
goto var_2621

}
}
if var_2628, ok := var_2620.(Tag__no_next_elem); ok {
_ = var_2628

if true {
break label_2617
goto var_2621

}
}

                    var_2621:
                    if false {}

var_2629 = var_2619
}
} else {
break label_2617
}
}
var var_2630 bool
_ = var_2630
var_2630 = false
var_2631 = var_2630
}
return var_2631
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) all(f func(e *Struct_std_____path_____FilePath) bool) bool {
_ = f
var Δorg_addr *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____Iter_____Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2648 bool
_ = var_2648
{
if false {goto label_2633}
label_2633:
for {
var var_2632 bool
_ = var_2632
var_2632 = true
if var_2632 {
var var_2646 Tup_
var_2646 = Tup_{}
_ = var_2646
{
var var_2634 any
_ = var_2634
var_2634 = (*self).next_fn()
var var_2635 Tup_
var_2635 = Tup_{}
_ = var_2635

var var_2636 any
_ = var_2636
var_2636 = var_2634
if var_2638, ok := var_2636.(*Struct_std_____path_____FilePath); ok {
_ = var_2638

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2636))
                            _ = iface_ptr

var t *Struct_std_____path_____FilePath
_ = t
t = Struct_std_____path_____FilePath_Copy((var_2638))
if true {
var var_2644 Tup_
var_2644 = Tup_{}
_ = var_2644
{
var var_2639 *Struct_std_____path_____FilePath
_ = var_2639
var_2639 = Struct_std_____path_____FilePath_Copy((t))
var var_2640 bool
_ = var_2640
var_2640 = f(var_2639)
var var_2641 bool
_ = var_2641
var_2641 = !var_2640
var var_2642 Tup_
var_2642 = Tup_{}
_ = var_2642
if var_2641 {
var var_2643 bool
_ = var_2643
var_2643 = false
return var_2643
}
var_2644 = var_2642
}
var_2635 = var_2644
goto var_2637

}
}
if var_2645, ok := var_2636.(Tag__no_next_elem); ok {
_ = var_2645

if true {
break label_2633
goto var_2637

}
}

                    var_2637:
                    if false {}

var_2646 = var_2635
}
} else {
break label_2633
}
}
var var_2647 bool
_ = var_2647
var_2647 = true
var_2648 = var_2647
}
return var_2648
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____col_____Iter_____Struct_std_____path_____FilePath_Copy(self *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath) *Struct_std_____col_____Iter_____Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____col_____Iter_____Struct_std_____path_____FilePath{
next_fn: IDENTITY((self.next_fn)),
}

}
type Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string struct {
inner any
}


type Struct_std_____opt_____Some_____Duck_exitcode_int_stderr_string_stdout_string struct {
value interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
}


type Struct_std_____opt_____Opt_____Tup_ struct {
inner any
}


type Struct_std_____opt_____Some_____Tup_ struct {
value Tup_
}


type Struct_std_____result_____Err_____Tup_ struct {
err Tup_
}

func  Struct_std_____result_____Err_____Tup__Copy(self *Struct_std_____result_____Err_____Tup_) *Struct_std_____result_____Err_____Tup_ {
_ = self
return &Struct_std_____result_____Err_____Tup_{
err: (self.err).copy(),
}

}
type Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_ struct {
value any
}



func (duck_internal_self *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_) ok() *Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string {

var self **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = self
self = &duck_internal_self
var var_2657 *Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string
_ = var_2657
{
var var_2649 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2649
var_2649 = IDENTITY((self))
var var_2650 *Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string
_ = var_2650

var var_2651 any
_ = var_2651
var_2651 = (*var_2649).value
if var_2653, ok := var_2651.(interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}); ok {
_ = var_2653

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2651))
                            _ = iface_ptr

var value interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = value
value = IDENTITY((var_2653))
if true {
var var_2654 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2654
var_2654 = IDENTITY((value))
var var_2655 *Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string
_ = var_2655
var_2655 = std_____opt_____Opt_____some_____Duck_exitcode_int_stderr_string_stdout_string(var_2654)
var_2650 = var_2655
goto var_2652

}
}
{
var var_2656 *Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string
_ = var_2656
var_2656 = std_____opt_____Opt_____none_____Duck_exitcode_int_stderr_string_stdout_string()
var_2650 = var_2656
}

                    var_2652:
                    if false {}

var_2657 = var_2650
}
return var_2657
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Duck_exitcode_int_stderr_string_stdout_string
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_) err() *Struct_std_____opt_____Opt_____Tup_ {

var self **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = self
self = &duck_internal_self
var var_2666 *Struct_std_____opt_____Opt_____Tup_
_ = var_2666
{
var var_2658 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2658
var_2658 = IDENTITY((self))
var var_2659 *Struct_std_____opt_____Opt_____Tup_
_ = var_2659

var var_2660 any
_ = var_2660
var_2660 = (*var_2658).value
if var_2662, ok := var_2660.(*Struct_std_____result_____Err_____Tup_); ok {
_ = var_2662

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2660))
                            _ = iface_ptr

var e *Struct_std_____result_____Err_____Tup_
_ = e
e = Struct_std_____result_____Err_____Tup__Copy((var_2662))
if true {
var var_2663 *Struct_std_____result_____Err_____Tup_
_ = var_2663
var_2663 = Struct_std_____result_____Err_____Tup__Copy((e))
var var_2664 *Struct_std_____opt_____Opt_____Tup_
_ = var_2664
var_2664 = std_____opt_____Opt_____some_____Tup_(var_2663.err)
var_2659 = var_2664
goto var_2661

}
}
{
var var_2665 *Struct_std_____opt_____Opt_____Tup_
_ = var_2665
var_2665 = std_____opt_____Opt_____none_____Tup_()
var_2659 = var_2665
}

                    var_2661:
                    if false {}

var_2666 = var_2659
}
return var_2666
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Tup_
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_) if_ok(f func(*interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}) Tup_) *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_ {
_ = f
var self **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = self
self = &duck_internal_self
var var_2678 *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2678
{
var var_2667 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2667
var_2667 = IDENTITY((self))
var var_2668 Tup_
var_2668 = Tup_{}
_ = var_2668

var var_2669 any
_ = var_2669
var_2669 = (*var_2667).value
if var_2671, ok := var_2669.(interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}); ok {
_ = var_2671

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2669))
                            _ = iface_ptr

var value interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = value
value = IDENTITY((var_2671))
if true {
var var_2672 *interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2672
var_2672 = &value
var var_2673 Tup_
var_2673 = Tup_{}
_ = var_2673
var_2673 = f(var_2672)
var_2668 = var_2673
goto var_2670

}
}
if var_2674, ok := var_2669.(*Struct_std_____result_____Err_____Tup_); ok {
_ = var_2674

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2669))
                            _ = iface_ptr

var err *Struct_std_____result_____Err_____Tup_
_ = err
err = Struct_std_____result_____Err_____Tup__Copy((var_2674))
if true {
var var_2675 Tup_
var_2675 = Tup_{}
_ = var_2675
{
var_2675 = Tup_{}
}
var_2668 = var_2675
goto var_2670

}
}

                    var_2670:
                    if false {}

var var_2676 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2676
var_2676 = IDENTITY((self))
var var_2677 *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2677
var_2677 = *var_2676
var_2678 = var_2677
}
return var_2678
var ΔΔΔretΔΔΔ **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_) if_err(f func(*Tup_) Tup_) *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_ {
_ = f
var self **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = self
self = &duck_internal_self
var var_2690 *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2690
{
var var_2679 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2679
var_2679 = IDENTITY((self))
var var_2680 Tup_
var_2680 = Tup_{}
_ = var_2680

var var_2681 any
_ = var_2681
var_2681 = (*var_2679).value
if var_2683, ok := var_2681.(interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}); ok {
_ = var_2683

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2681))
                            _ = iface_ptr

var value interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = value
value = IDENTITY((var_2683))
if true {
var var_2684 Tup_
var_2684 = Tup_{}
_ = var_2684
{
var_2684 = Tup_{}
}
var_2680 = var_2684
goto var_2682

}
}
if var_2685, ok := var_2681.(*Struct_std_____result_____Err_____Tup_); ok {
_ = var_2685

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2681))
                            _ = iface_ptr

var e *Struct_std_____result_____Err_____Tup_
_ = e
e = Struct_std_____result_____Err_____Tup__Copy((var_2685))
if true {
var var_2686 *Tup_
_ = var_2686
var_2686 = &e.err
var var_2687 Tup_
var_2687 = Tup_{}
_ = var_2687
var_2687 = f(var_2686)
var_2680 = var_2687
goto var_2682

}
}

                    var_2682:
                    if false {}

var var_2688 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2688
var_2688 = IDENTITY((self))
var var_2689 *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2689
var_2689 = *var_2688
var_2690 = var_2689
}
return var_2690
var ΔΔΔretΔΔΔ **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_) expect(msg string) interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
} {
_ = msg
var self **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = self
self = &duck_internal_self
var var_2698 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2698
{
var var_2691 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2691
var_2691 = IDENTITY((self))
var var_2692 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2692

var var_2693 any
_ = var_2693
var_2693 = (*var_2691).value
if var_2695, ok := var_2693.(interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}); ok {
_ = var_2695

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2693))
                            _ = iface_ptr

var value interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = value
value = IDENTITY((var_2695))
if true {
var var_2696 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2696
var_2696 = IDENTITY((value))
var_2692 = var_2696
goto var_2694

}
}
{
var var_2697 string
_ = var_2697
var_2697 = IDENTITY((msg))
std_____error_____panic(var_2697)
}

                    var_2694:
                    if false {}

var_2698 = var_2692
}
return var_2698
var ΔΔΔretΔΔΔ *interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_) expect_err(msg string) Tup_ {
_ = msg
var self **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = self
self = &duck_internal_self
var var_2706 Tup_
var_2706 = Tup_{}
_ = var_2706
{
var var_2699 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2699
var_2699 = IDENTITY((self))
var var_2700 Tup_
var_2700 = Tup_{}
_ = var_2700

var var_2701 any
_ = var_2701
var_2701 = (*var_2699).value
if var_2703, ok := var_2701.(*Struct_std_____result_____Err_____Tup_); ok {
_ = var_2703

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2701))
                            _ = iface_ptr

var e *Struct_std_____result_____Err_____Tup_
_ = e
e = Struct_std_____result_____Err_____Tup__Copy((var_2703))
if true {
var var_2704 *Struct_std_____result_____Err_____Tup_
_ = var_2704
var_2704 = Struct_std_____result_____Err_____Tup__Copy((e))
var_2700 = var_2704.err
goto var_2702

}
}
{
var var_2705 string
_ = var_2705
var_2705 = IDENTITY((msg))
std_____error_____panic(var_2705)
}

                    var_2702:
                    if false {}

var_2706 = var_2700
}
return var_2706
return Tup_{}
}

func (duck_internal_self *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_) or_else(default_value interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}) interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
} {
_ = default_value
var self **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = self
self = &duck_internal_self
var var_2714 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2714
{
var var_2707 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2707
var_2707 = IDENTITY((self))
var var_2708 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2708

var var_2709 any
_ = var_2709
var_2709 = (*var_2707).value
if var_2711, ok := var_2709.(interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}); ok {
_ = var_2711

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2709))
                            _ = iface_ptr

var value interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = value
value = IDENTITY((var_2711))
if true {
var var_2712 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2712
var_2712 = IDENTITY((value))
var_2708 = var_2712
goto var_2710

}
}
{
var var_2713 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2713
var_2713 = IDENTITY((default_value))
var_2708 = var_2713
}

                    var_2710:
                    if false {}

var_2714 = var_2708
}
return var_2714
var ΔΔΔretΔΔΔ *interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_) or_else_get(default_supplier func() interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}) interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
} {
_ = default_supplier
var self **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = self
self = &duck_internal_self
var var_2722 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2722
{
var var_2715 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2715
var_2715 = IDENTITY((self))
var var_2716 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2716

var var_2717 any
_ = var_2717
var_2717 = (*var_2715).value
if var_2719, ok := var_2717.(interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}); ok {
_ = var_2719

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2717))
                            _ = iface_ptr

var value interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = value
value = IDENTITY((var_2719))
if true {
var var_2720 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2720
var_2720 = IDENTITY((value))
var_2716 = var_2720
goto var_2718

}
}
{
var var_2721 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2721
var_2721 = default_supplier()
var_2716 = var_2721
}

                    var_2718:
                    if false {}

var_2722 = var_2716
}
return var_2722
var ΔΔΔretΔΔΔ *interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_) unwrap() interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
} {

var self **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = self
self = &duck_internal_self
var var_2730 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2730
{
var var_2723 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2723
var_2723 = IDENTITY((self))
var var_2724 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2724

var var_2725 any
_ = var_2725
var_2725 = (*var_2723).value
if var_2727, ok := var_2725.(interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}); ok {
_ = var_2727

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2725))
                            _ = iface_ptr

var value interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = value
value = IDENTITY((var_2727))
if true {
var var_2728 interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
_ = var_2728
var_2728 = IDENTITY((value))
var_2724 = var_2728
goto var_2726

}
}
{
var var_2729 string
_ = var_2729
var_2729 = "called unwrap on Err"
std_____error_____panic(var_2729)
}

                    var_2726:
                    if false {}

var_2730 = var_2724
}
return var_2730
var ΔΔΔretΔΔΔ *interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_) unwrap_err() Tup_ {

var self **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = self
self = &duck_internal_self
var var_2738 Tup_
var_2738 = Tup_{}
_ = var_2738
{
var var_2731 **Struct_std_____result_____Result_____Duck_exitcode_int_stderr_string_stdout_string_____Tup_
_ = var_2731
var_2731 = IDENTITY((self))
var var_2732 Tup_
var_2732 = Tup_{}
_ = var_2732

var var_2733 any
_ = var_2733
var_2733 = (*var_2731).value
if var_2735, ok := var_2733.(*Struct_std_____result_____Err_____Tup_); ok {
_ = var_2735

                            iface_ptr := (*go_iface)(unsafe.Pointer(&var_2733))
                            _ = iface_ptr

var e *Struct_std_____result_____Err_____Tup_
_ = e
e = Struct_std_____result_____Err_____Tup__Copy((var_2735))
if true {
var var_2736 *Struct_std_____result_____Err_____Tup_
_ = var_2736
var_2736 = Struct_std_____result_____Err_____Tup__Copy((e))
var_2732 = var_2736.err
goto var_2734

}
}
{
var var_2737 string
_ = var_2737
var_2737 = "called unwrap_err on Ok"
std_____error_____panic(var_2737)
}

                    var_2734:
                    if false {}

var_2738 = var_2732
}
return var_2738
return Tup_{}
}

type Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath struct {
inner any
}


type Struct_std_____opt_____Some_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath struct {
value interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
}


type Struct_std_____col_____Iter_____Ref___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____col_____Iter_____RefMut___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath struct {
next_fn func() any
}


type Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath struct {
elems []interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
}



func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) push(new_elem interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}) **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {
_ = new_elem
var Δorg_addr *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2739 Tup_
var_2739 = Tup_{}
_ = var_2739

            (*self).elems = append((*self).elems, new_elem)

var var_2740 **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_2740
var_2740 = IDENTITY((self))
return var_2740
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) pop() *Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2741 int
_ = var_2741
var_2741 = (*self).len()
var var_2742 int
_ = var_2742
var_2742 = 0
var var_2743 bool
_ = var_2743
switch Int_Ord((var_2741), &(var_2742)).(type) {
                            case Tag__greater:
                            var_2743 = true
                            default:
                            var_2743 = false
                            }

var var_2744 Tup_
var_2744 = Tup_{}
_ = var_2744
if var_2743 {
var var_2745 interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2745

                var index = len((*self).elems) - 1;
                var_2745 = (*self).elems[index];
                (*self).elems = (*self).elems[:index];

var var_2746 interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2746
var_2746 = var_2745
var value interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = value
value = var_2746
var var_2747 interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2747
var_2747 = IDENTITY((value))
var var_2748 *Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_2748
var_2748 = std_____opt_____Opt_____some_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath(var_2747)
return var_2748
}
var var_2749 *Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_2749
var_2749 = std_____opt_____Opt_____none_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath()
return var_2749
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) get(index int) *Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {
_ = index
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var tag_none *Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = tag_none
tag_none = std_____opt_____Opt_____none_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath()
var var_2750 interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2750

            if index < 0 || index >= (*self).len() {
                return tag_none;
            }

            var_2750 = (*self).elems[index]

var var_2751 interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2751
var_2751 = var_2750
var elem interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = elem
elem = var_2751
var var_2752 interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2752
var_2752 = IDENTITY((elem))
var var_2753 *Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_2753
var_2753 = std_____opt_____Opt_____some_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath(var_2752)
return var_2753
var ΔΔΔretΔΔΔ **Struct_std_____opt_____Opt_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) set(index int, value interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}) **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {
_ = index
_ = value
var Δorg_addr *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2754 Tup_
var_2754 = Tup_{}
_ = var_2754

            (*self).elems[index] = value

var var_2755 **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_2755
var_2755 = IDENTITY((self))
return var_2755
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) remove(index int) **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {
_ = index
var Δorg_addr *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2756 Tup_
var_2756 = Tup_{}
_ = var_2756

            (*self).elems = append((*self).elems[:index], (*self).elems[index+1:]...)

var var_2757 **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_2757
var_2757 = IDENTITY((self))
return var_2757
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) len() int {

var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2758 int
_ = var_2758
var_2758 = 0
var l int
_ = l
l = var_2758
var var_2759 Tup_
var_2759 = Tup_{}
_ = var_2759

            l = len((*self).elems);

var var_2760 int
_ = var_2760
var_2760 = IDENTITY((l))
return var_2760
var ΔΔΔretΔΔΔ *int
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) is_empty() bool {

var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2761 bool
_ = var_2761
var_2761 = true
var empty bool
_ = empty
empty = var_2761
var var_2762 Tup_
var_2762 = Tup_{}
_ = var_2762

            empty = len((*self).elems) == 0

var var_2763 bool
_ = var_2763
var_2763 = IDENTITY((empty))
return var_2763
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) clear() **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2764 []interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2764
var_2764 = []interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}{}
var var_2765 []interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2765
var_2765 = var_2764
(*self).elems = var_2765
var var_2766 **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_2766
var_2766 = IDENTITY((self))
return var_2766
var ΔΔΔretΔΔΔ ***Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) filter(f func(x interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}) bool) *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {
_ = f
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var res *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = res
res = std_____col_____ArrayList_____new_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath()
var var_2767 Tup_
var_2767 = Tup_{}
_ = var_2767

            for _, e := range (*self).elems {
                if f(e) {
                    res.elems = append(res.elems, e)
                }
            }

var var_2768 *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_2768
var_2768 = Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath_Copy((res))
return var_2768
var ΔΔΔretΔΔΔ **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) for_each(f func(e interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}) Tup_) Tup_ {
_ = f
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2770 Tup_
var_2770 = Tup_{}
_ = var_2770
{
var var_2769 Tup_
var_2769 = Tup_{}
_ = var_2769

            for _, e := range (*self).elems {
                f(e)
            }

var_2770 = var_2769
}
return var_2770
return Tup_{}
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) find(f func(x interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}) bool) any {
_ = f
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2771 Tag__none
_ = var_2771
var_2771 = Tag__none{}
var result any
_ = result
result = var_2771
var var_2772 Tup_
var_2772 = Tup_{}
_ = var_2772

            for _, e := range (*self).elems {
                if f(e) {
                    result = e
                    break
                }
            }

var var_2773 any
_ = var_2773
var_2773 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}:
                            tmp := p1.(interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
})
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_2773
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) contains(something interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}) bool {
_ = something
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2774 bool
_ = var_2774
var_2774 = false
var result bool
_ = result
result = var_2774
var var_2775 Tup_
var_2775 = Tup_{}
_ = var_2775

            for _, e := range (*self).elems {
                if e == something {
                    result = true
                    break
                }
            }

var var_2776 bool
_ = var_2776
var_2776 = IDENTITY((result))
return var_2776
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) any(f func(x interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}) bool) bool {
_ = f
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2777 bool
_ = var_2777
var_2777 = false
var result bool
_ = result
result = var_2777
var var_2778 Tup_
var_2778 = Tup_{}
_ = var_2778

            for _, e := range (*self).elems {
                if f(e) {
                    result = true
                    break
                }
            }

var var_2779 bool
_ = var_2779
var_2779 = IDENTITY((result))
return var_2779
var ΔΔΔretΔΔΔ *bool
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) find_index(predicate func(element interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}) bool) any {
_ = predicate
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2780 Tag__none
_ = var_2780
var_2780 = Tag__none{}
var result any
_ = result
result = var_2780
var var_2781 Tup_
var_2781 = Tup_{}
_ = var_2781

            for i, elem := range (*self).elems {
                if predicate(elem) {
                    result = i;
                    break
                }
            }

var var_2782 any
_ = var_2782
var_2782 = func() any {
                    var p1 any = (result)

                        switch p1.(type) {
                        case int:
                            tmp := p1.(int)
                            _ = tmp
                            return IDENTITY((tmp))
                        }

                        switch p1.(type) {
                        case Tag__none:
                            tmp := p1.(Tag__none)
                            _ = tmp
                            return IDENTITY((tmp))
                        }
                    var ret_guard *any
return *ret_guard }()
return var_2782
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) iter() *Struct_std_____col_____Iter_____Ref___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {

var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2783 int
_ = var_2783
var_2783 = 0
var idx int
_ = idx
idx = var_2783
var var_2784 func() any
_ = var_2784
var_2784 = func() any {
var var_2785 int
_ = var_2785
var_2785 = IDENTITY((idx))
var var_2786 int
_ = var_2786
var_2786 = (*self).len()
var var_2787 bool
_ = var_2787
switch Int_Ord((var_2785), &(var_2786)).(type) {
                            case Tag__greater:
                            var_2787 = true
                            case Tag__equal:
                            var_2787 = true
                            default:
                            var_2787 = false
                            }

var var_2788 Tup_
var_2788 = Tup_{}
_ = var_2788
if var_2787 {
var var_2789 Tag__no_next_elem
_ = var_2789
var_2789 = Tag__no_next_elem{}
return var_2789
}
var var_2790 int
_ = var_2790
var_2790 = IDENTITY((idx))
var var_2791 *interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2791
var_2791 = &(*self).elems[var_2790]
var elem_to_ret *interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = elem_to_ret
elem_to_ret = var_2791
var var_2792 int
_ = var_2792
var_2792 = IDENTITY((idx))
var var_2793 int
_ = var_2793
var_2793 = 1
var var_2794 int
_ = var_2794
var_2794 = var_2792 + var_2793
idx = var_2794
var var_2795 *interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2795
var_2795 = IDENTITY((elem_to_ret))
return var_2795
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_2784
var var_2796 func() any
_ = var_2796
var_2796 = IDENTITY((f))
var var_2797 *Struct_std_____col_____Iter_____Ref___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_2797
var_2797 = std_____col_____Iter_____from_____Ref___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath(var_2796)
return var_2797
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____Ref___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) iter_mut() *Struct_std_____col_____Iter_____RefMut___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {

var Δorg_addr *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2798 int
_ = var_2798
var_2798 = 0
var idx int
_ = idx
idx = var_2798
var var_2799 func() any
_ = var_2799
var_2799 = func() any {
var var_2800 int
_ = var_2800
var_2800 = IDENTITY((idx))
var var_2801 int
_ = var_2801
var_2801 = (*self).len()
var var_2802 bool
_ = var_2802
switch Int_Ord((var_2800), &(var_2801)).(type) {
                            case Tag__greater:
                            var_2802 = true
                            case Tag__equal:
                            var_2802 = true
                            default:
                            var_2802 = false
                            }

var var_2803 Tup_
var_2803 = Tup_{}
_ = var_2803
if var_2802 {
var var_2804 Tag__no_next_elem
_ = var_2804
var_2804 = Tag__no_next_elem{}
return var_2804
}
var var_2805 int
_ = var_2805
var_2805 = IDENTITY((idx))
var var_2806 *interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2806
var_2806 = &(*self).elems[var_2805]
var elem_to_ret *interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = elem_to_ret
elem_to_ret = var_2806
var var_2807 int
_ = var_2807
var_2807 = IDENTITY((idx))
var var_2808 int
_ = var_2808
var_2808 = 1
var var_2809 int
_ = var_2809
var_2809 = var_2807 + var_2808
idx = var_2809
var var_2810 *interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2810
var_2810 = IDENTITY((elem_to_ret))
return var_2810
var ΔΔΔretΔΔΔ *any
return *ΔΔΔretΔΔΔ
}
var f func() any
_ = f
f = var_2799
var var_2811 func() any
_ = var_2811
var_2811 = IDENTITY((f))
var var_2812 *Struct_std_____col_____Iter_____RefMut___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = var_2812
var_2812 = std_____col_____Iter_____from_____RefMut___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath(var_2811)
return var_2812
var ΔΔΔretΔΔΔ **Struct_std_____col_____Iter_____RefMut___Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) as_ref() *[]interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
} {

var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
var var_2813 *[]interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2813
var_2813 = &(*self).elems
return var_2813
var ΔΔΔretΔΔΔ **[]interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
return *ΔΔΔretΔΔΔ
}

func (duck_internal_self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) as_mut() *[]interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
} {

var Δorg_addr *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = Δorg_addr
Δorg_addr = duck_internal_self
var self **Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath
_ = self
self = &duck_internal_self
defer func() { *Δorg_addr = **self }()
var var_2814 *[]interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
_ = var_2814
var_2814 = &(*self).elems
return var_2814
var ΔΔΔretΔΔΔ **[]interface {
   Hasbinary_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hasdargo_output[interface {
   Hasexitcode[int]
   Hasstderr[string]
   Hasstdout[string]
}]
   Hassrc_path[*Struct_std_____path_____FilePath]
}
return *ΔΔΔretΔΔΔ
}
func  Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath_Copy(self *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath) *Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath {
_ = self
return &Struct_std_____col_____ArrayList_____Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath{
elems: Array_Duck_binary_output_Duck_exitcode_int_stderr_string_stdout_string_dargo_output_Duck_exitcode_int_stderr_string_stdout_string_src_path_Struct_std_____path_____FilePath_Copy((self.elems)),
}

}



func  Int_Hash(self int) int {
_ = self
return self
}


func  Int_Ord(self int, other *int) any {
_ = self
_ = other
if self < *other { return Tag__smaller{} } else if self > *other { return Tag__greater{} } else { return Tag__equal{} }
}


func  String_Hash(self string) int {
_ = self

                    var h maphash.Hash
                    h.WriteString(self)
                    return int(h.Sum64())

}

func  String_Ord(self string, other_param *string) any {
_ = self
_ = other_param

                other := *other_param
                if len(self) < len(other) {
                    return Tag__smaller{}
                } else if len(self) > len(other) {
                    return Tag__greater{}
                } else {
                    runes_self := []rune(self)
                    runes_other := []rune(self)

                    for i := range runes_self {
                        if runes_self[i] < runes_other[i] {
                            return Tag__smaller{}
                        } else if runes_self[i] > runes_other[i] {
                            return Tag__greater{}
                        }
                    }

                    return Tag__equal{}
                }

}


func IDENTITY[T any](x T) T {
_ = x
return x
}

// package main

// import (
// 	"encoding/json"
// 	"errors"
// 	"fmt"
// 	"log"
// 	"strings"
// 	"unicode/utf8"
// )

// func main() {
// 	r, _, _ := scan_json_array_parts("[[[],[]],\"hallo\",\"ayooo\",    \"fawf\", 'a']")
// 	_ = r
// 	x, _, e := scan_json_struct_parts("{\"yo\":[1,-2,true], \"a\":[],\"d\": {\"xx\":{\"a\": 1}},\"yo2\":123}")
// 	if e != nil {
// 		log.Fatal(e)
// 	}
// 	fmt.Println(object_to_string(x))
// }
//

type go_iface struct {
	t unsafe.Pointer
	v unsafe.Pointer
}




const TAILWIND_STR = "/*\n  1. Prevent padding and border from affecting element width. (https://github.com/mozdevs/cssremedy/issues/4)\n  2. Remove default margins and padding\n  3. Reset all borders.\n*/\n\n*,\n::after,\n::before,\n::backdrop,\n::file-selector-button {\n  box-sizing: border-box; /* 1 */\n  margin: 0; /* 2 */\n  padding: 0; /* 2 */\n  border: 0 solid; /* 3 */\n}\n\n/*\n  1. Use a consistent sensible line-height in all browsers.\n  2. Prevent adjustments of font size after orientation changes in iOS.\n  3. Use a more readable tab size.\n  4. Use the user's configured `sans` font-family by default.\n  5. Use the user's configured `sans` font-feature-settings by default.\n  6. Use the user's configured `sans` font-variation-settings by default.\n  7. Disable tap highlights on iOS.\n*/\n\nhtml,\n:host {\n  line-height: 1.5; /* 1 */\n  -webkit-text-size-adjust: 100%; /* 2 */\n  tab-size: 4; /* 3 */\n  font-family:\n    var(--default-font-family),\n    ui-sans-serif,\n    system-ui,\n    sans-serif,\n    'Apple Color Emoji',\n    'Segoe UI Emoji',\n    'Segoe UI Symbol',\n    'Noto Color Emoji'; /* 4 */\n  font-feature-settings: var(--default-font-feature-settings, normal); /* 5 */\n  font-variation-settings: var(--default-font-variation-settings, normal); /* 6 */\n  -webkit-tap-highlight-color: transparent; /* 7 */\n}\n\n/*\n  1. Add the correct height in Firefox.\n  2. Correct the inheritance of border color in Firefox. (https://bugzilla.mozilla.org/show_bug.cgi?id=190655)\n  3. Reset the default border style to a 1px solid border.\n*/\n\nhr {\n  height: 0; /* 1 */\n  color: inherit; /* 2 */\n  border-top-width: 1px; /* 3 */\n}\n\n/*\n  Add the correct text decoration in Chrome, Edge, and Safari.\n*/\n\nabbr:where([title]) {\n  -webkit-text-decoration: underline dotted;\n  text-decoration: underline dotted;\n}\n\n/*\n  Remove the default font size and weight for headings.\n*/\n\nh1,\nh2,\nh3,\nh4,\nh5,\nh6 {\n  font-size: inherit;\n  font-weight: inherit;\n}\n\n/*\n  Reset links to optimize for opt-in styling instead of opt-out.\n*/\n\na {\n  color: inherit;\n  -webkit-text-decoration: inherit;\n  text-decoration: inherit;\n}\n\n/*\n  Add the correct font weight in Edge and Safari.\n*/\n\nb,\nstrong {\n  font-weight: bolder;\n}\n\n/*\n  1. Use the user's configured `mono` font-family by default.\n  2. Use the user's configured `mono` font-feature-settings by default.\n  3. Use the user's configured `mono` font-variation-settings by default.\n  4. Correct the odd `em` font sizing in all browsers.\n*/\n\ncode,\nkbd,\nsamp,\npre {\n  font-family: var(\n    --default-mono-font-family),\n    ui-monospace,\n    SFMono-Regular,\n    Menlo,\n    Monaco,\n    Consolas,\n    'Liberation Mono',\n    'Courier New',\n    monospace; /* 1 */\n  font-feature-settings: var(--default-mono-font-feature-settings, normal); /* 2 */\n  font-variation-settings: var(--default-mono-font-variation-settings, normal); /* 3 */\n  font-size: 1em; /* 4 */\n}\n\n/*\n  Add the correct font size in all browsers.\n*/\n\nsmall {\n  font-size: 80%;\n}\n\n/*\n  Prevent `sub` and `sup` elements from affecting the line height in all browsers.\n*/\n\nsub,\nsup {\n  font-size: 75%;\n  line-height: 0;\n  position: relative;\n  vertical-align: baseline;\n}\n\nsub {\n  bottom: -0.25em;\n}\n\nsup {\n  top: -0.5em;\n}\n\n/*\n  1. Remove text indentation from table contents in Chrome and Safari. (https://bugs.chromium.org/p/chromium/issues/detail?id=999088, https://bugs.webkit.org/show_bug.cgi?id=201297)\n  2. Correct table border color inheritance in all Chrome and Safari. (https://bugs.chromium.org/p/chromium/issues/detail?id=935729, https://bugs.webkit.org/show_bug.cgi?id=195016)\n  3. Remove gaps between table borders by default.\n*/\n\ntable {\n  text-indent: 0; /* 1 */\n  border-color: inherit; /* 2 */\n  border-collapse: collapse; /* 3 */\n}\n\n/*\n  Use the modern Firefox focus style for all focusable elements.\n*/\n\n:-moz-focusring {\n  outline: auto;\n}\n\n/*\n  Add the correct vertical alignment in Chrome and Firefox.\n*/\n\nprogress {\n  vertical-align: baseline;\n}\n\n/*\n  Add the correct display in Chrome and Safari.\n*/\n\nsummary {\n  display: list-item;\n}\n\n/*\n  Make lists unstyled by default.\n*/\n\nol,\nul,\nmenu {\n  list-style: none;\n}\n\n/*\n  1. Make replaced elements `display: block` by default. (https://github.com/mozdevs/cssremedy/issues/14)\n  2. Add `vertical-align: middle` to align replaced elements more sensibly by default. (https://github.com/jensimmons/cssremedy/issues/14#issuecomment-634934210)\n      This can trigger a poorly considered lint error in some tools but is included by design.\n*/\n\nimg,\nsvg,\nvideo,\ncanvas,\naudio,\niframe,\nembed,\nobject {\n  display: block; /* 1 */\n  vertical-align: middle; /* 2 */\n}\n\n/*\n  Constrain images and videos to the parent width and preserve their intrinsic aspect ratio. (https://github.com/mozdevs/cssremedy/issues/14)\n*/\n\nimg,\nvideo {\n  max-width: 100%;\n  height: auto;\n}\n\n/*\n  1. Inherit font styles in all browsers.\n  2. Remove border radius in all browsers.\n  3. Remove background color in all browsers.\n  4. Ensure consistent opacity for disabled states in all browsers.\n*/\n\nbutton,\ninput,\nselect,\noptgroup,\ntextarea,\n::file-selector-button {\n  font: inherit; /* 1 */\n  font-feature-settings: inherit; /* 1 */\n  font-variation-settings: inherit; /* 1 */\n  letter-spacing: inherit; /* 1 */\n  color: inherit; /* 1 */\n  border-radius: 0; /* 2 */\n  background-color: transparent; /* 3 */\n  opacity: 1; /* 4 */\n}\n\n/*\n  Restore default font weight.\n*/\n\n:where(select:is([multiple], [size])) optgroup {\n  font-weight: bolder;\n}\n\n/*\n  Restore indentation.\n*/\n\n:where(select:is([multiple], [size])) optgroup option {\n  padding-inline-start: 20px;\n}\n\n/*\n  Restore space after button.\n*/\n\n::file-selector-button {\n  margin-inline-end: 4px;\n}\n\n/*\n  Reset the default placeholder opacity in Firefox. (https://github.com/tailwindlabs/tailwindcss/issues/3300)\n*/\n\n::placeholder {\n  opacity: 1;\n}\n\n/*\n  Set the default placeholder color to a semi-transparent version of the current text color in browsers that do not\n  crash when using `color-mix(…)` with `currentcolor`. (https://github.com/tailwindlabs/tailwindcss/issues/17194)\n*/\n\n@supports (not (-webkit-appearance: -apple-pay-button)) /* Not Safari */ or\n  (contain-intrinsic-size: 1px) /* Safari 17+ */ {\n  ::placeholder {\n    color: color-mix(in oklab, currentcolor 50%, transparent);\n  }\n}\n\n/*\n  Prevent resizing textareas horizontally by default.\n*/\n\ntextarea {\n  resize: vertical;\n}\n\n/*\n  Remove the inner padding in Chrome and Safari on macOS.\n*/\n\n::-webkit-search-decoration {\n  -webkit-appearance: none;\n}\n\n/*\n  1. Ensure date/time inputs have the same height when empty in iOS Safari.\n  2. Ensure text alignment can be changed on date/time inputs in iOS Safari.\n*/\n\n::-webkit-date-and-time-value {\n  min-height: 1lh; /* 1 */\n  text-align: inherit; /* 2 */\n}\n\n/*\n  Prevent height from changing on date/time inputs in macOS Safari when the input is set to `display: block`.\n*/\n\n::-webkit-datetime-edit {\n  display: inline-flex;\n}\n\n/*\n  Remove excess padding from pseudo-elements in date/time inputs to ensure consistent height across browsers.\n*/\n\n::-webkit-datetime-edit-fields-wrapper {\n  padding: 0;\n}\n\n::-webkit-datetime-edit,\n::-webkit-datetime-edit-year-field,\n::-webkit-datetime-edit-month-field,\n::-webkit-datetime-edit-day-field,\n::-webkit-datetime-edit-hour-field,\n::-webkit-datetime-edit-minute-field,\n::-webkit-datetime-edit-second-field,\n::-webkit-datetime-edit-millisecond-field,\n::-webkit-datetime-edit-meridiem-field {\n  padding-block: 0;\n}\n\n/*\n  Center dropdown marker shown on inputs with paired `<datalist>`s in Chrome. (https://github.com/tailwindlabs/tailwindcss/issues/18499)\n*/\n\n::-webkit-calendar-picker-indicator {\n  line-height: 1;\n}\n\n/*\n  Remove the additional `:invalid` styles in Firefox. (https://github.com/mozilla/gecko-dev/blob/2f9eacd9d3d995c937b4251a5557d95d494c9be1/layout/style/res/forms.css#L728-L737)\n*/\n\n:-moz-ui-invalid {\n  box-shadow: none;\n}\n\n/*\n  Correct the inability to style the border radius in iOS Safari.\n*/\n\nbutton,\ninput:where([type='button'], [type='reset'], [type='submit']),\n::file-selector-button {\n  appearance: button;\n}\n\n/*\n  Correct the cursor style of increment and decrement buttons in Safari.\n*/\n\n::-webkit-inner-spin-button,\n::-webkit-outer-spin-button {\n  height: auto;\n}\n\n/*\n  Make elements with the HTML hidden attribute stay hidden by default.\n*/\n\n[hidden]:where(:not([hidden='until-found'])) {\n  display: none !important;\n}\n\n@property --tw-translate-x {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: 0;\n}\n@property --tw-translate-y {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: 0;\n}\n@property --tw-translate-z {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: 0;\n}\n@property --tw-space-y-reverse {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: 0;\n}\n@property --tw-divide-y-reverse {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: 0;\n}\n@property --tw-border-style {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: solid;\n}\n@property --tw-gradient-position {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-gradient-from {\n  syntax: \"<color>\";\n  inherits: false;\n  initial-value: #0000;\n}\n@property --tw-gradient-via {\n  syntax: \"<color>\";\n  inherits: false;\n  initial-value: #0000;\n}\n@property --tw-gradient-to {\n  syntax: \"<color>\";\n  inherits: false;\n  initial-value: #0000;\n}\n@property --tw-gradient-stops {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-gradient-via-stops {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-gradient-from-position {\n  syntax: \"<length-percentage>\";\n  inherits: false;\n  initial-value: 0%;\n}\n@property --tw-gradient-via-position {\n  syntax: \"<length-percentage>\";\n  inherits: false;\n  initial-value: 50%;\n}\n@property --tw-gradient-to-position {\n  syntax: \"<length-percentage>\";\n  inherits: false;\n  initial-value: 100%;\n}\n@property --tw-font-weight {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-tracking {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-shadow {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: 0 0 #0000;\n}\n@property --tw-shadow-color {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-shadow-alpha {\n  syntax: \"<percentage>\";\n  inherits: false;\n  initial-value: 100%;\n}\n@property --tw-inset-shadow {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: 0 0 #0000;\n}\n@property --tw-inset-shadow-color {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-inset-shadow-alpha {\n  syntax: \"<percentage>\";\n  inherits: false;\n  initial-value: 100%;\n}\n@property --tw-ring-color {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-ring-shadow {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: 0 0 #0000;\n}\n@property --tw-inset-ring-color {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-inset-ring-shadow {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: 0 0 #0000;\n}\n@property --tw-ring-inset {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-ring-offset-width {\n  syntax: \"<length>\";\n  inherits: false;\n  initial-value: 0px;\n}\n@property --tw-ring-offset-color {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: #fff;\n}\n@property --tw-ring-offset-shadow {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: 0 0 #0000;\n}\n@property --tw-blur {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-brightness {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-contrast {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-grayscale {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-hue-rotate {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-invert {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-opacity {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-saturate {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-sepia {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-drop-shadow {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-drop-shadow-color {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-drop-shadow-alpha {\n  syntax: \"<percentage>\";\n  inherits: false;\n  initial-value: 100%;\n}\n@property --tw-drop-shadow-size {\n  syntax: \"*\";\n  inherits: false;\n}\n@property --tw-outline-style {\n  syntax: \"*\";\n  inherits: false;\n  initial-value: solid;\n}\n:root {\n--tracking-wide: 0.025em;\n--shadow: 0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1);\n--drop-shadow-sm: 0 1px 2px rgb(0 0 0 / 0.15);\n--animate-pulse: pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite;\n--color-orange-600: oklch(64.6% 0.222 41.116);\n--color-black: #000;\n--color-indigo-100: oklch(93% 0.034 272.788);\n--default-ring-width: 1px;\n--color-fuchsia-200: oklch(90.3% 0.076 319.62);\n--font-weight-light: 300;\n--color-slate-300: oklch(86.9% 0.022 252.894);\n--color-orange-50: oklch(98% 0.016 73.684);\n--color-yellow-700: oklch(55.4% 0.135 66.442);\n--color-green-500: oklch(72.3% 0.219 149.579);\n--color-purple-50: oklch(97.7% 0.014 308.299);\n--text-8xl: 6rem;\n--shadow-xl: 0 20px 25px -5px rgb(0 0 0 / 0.1), 0 8px 10px -6px rgb(0 0 0 / 0.1);\n--blur-lg: 16px;\n--color-pink-200: oklch(89.9% 0.061 343.231);\n--color-red-700: oklch(50.5% 0.213 27.518);\n--radius-lg: 0.5rem;\n--font-weight-extrabold: 800;\n--inset-shadow-2xs: inset 0 1px rgb(0 0 0 / 0.05);\n--color-indigo-800: oklch(39.8% 0.195 277.366);\n--max-width-prose: 65ch;\n--shadow-lg: 0 10px 15px -3px rgb(0 0 0 / 0.1), 0 4px 6px -4px rgb(0 0 0 / 0.1);\n--blur-xs: 4px;\n--drop-shadow-xl: 0 9px 7px rgb(0 0 0 / 0.1);\n--text-shadow-sm: 0px 1px 0px rgb(0 0 0 / 0.075), 0px 1px 1px rgb(0 0 0 / 0.075), 0px 2px 2px rgb(0 0 0 / 0.075);\n--color-purple-500: oklch(62.7% 0.265 303.9);\n--drop-shadow-lg: 0 4px 4px rgb(0 0 0 / 0.15);\n--color-neutral-50: oklch(98.5% 0 0);\n--color-yellow-950: oklch(28.6% 0.066 53.813);\n--default-font-feature-settings: var(--font-sans--font-feature-settings, initial);\n--blur-3xl: 64px;\n--color-sky-100: oklch(95.1% 0.026 236.824);\n--text-8xl--line-height: 1;\n--color-blue-500: oklch(62.3% 0.214 259.815);\n--color-cyan-200: oklch(91.7% 0.08 205.041);\n--text-xs--line-height: calc(1 / 0.75);\n--breakpoint-md: 48rem;\n--color-slate-50: oklch(98.4% 0.003 247.858);\n--default-mono-font-variation-settings: var(--font-mono--font-variation-settings, initial);\n--color-red-950: oklch(25.8% 0.092 26.042);\n--radius: 0.25rem;\n--text-shadow-2xs: 0px 1px 0px rgb(0 0 0 / 0.15);\n--color-red-300: oklch(80.8% 0.114 19.571);\n--color-sky-600: oklch(58.8% 0.158 241.966);\n--color-fuchsia-300: oklch(83.3% 0.145 321.434);\n--color-sky-200: oklch(90.1% 0.058 230.902);\n--color-sky-500: oklch(68.5% 0.169 237.323);\n--color-lime-900: oklch(40.5% 0.101 131.063);\n--text-9xl--line-height: 1;\n--color-blue-950: oklch(28.2% 0.091 267.935);\n--color-red-500: oklch(63.7% 0.237 25.331);\n--color-neutral-200: oklch(92.2% 0 0);\n--color-green-950: oklch(26.6% 0.065 152.934);\n--color-yellow-50: oklch(98.7% 0.026 102.212);\n--color-green-800: oklch(44.8% 0.119 151.328);\n--color-fuchsia-50: oklch(97.7% 0.017 320.058);\n--perspective-dramatic: 100px;\n--container-2xl: 42rem;\n--text-5xl--line-height: 1;\n--color-fuchsia-500: oklch(66.7% 0.295 322.15);\n--color-fuchsia-400: oklch(74% 0.238 322.16);\n--color-slate-100: oklch(96.8% 0.007 247.896);\n--default-ring-color: currentcolor;\n--default-font-variation-settings: var(--font-sans--font-variation-settings, initial);\n--color-teal-50: oklch(98.4% 0.014 180.72);\n--color-emerald-500: oklch(69.6% 0.17 162.48);\n--breakpoint-xl: 80rem;\n--color-neutral-500: oklch(55.6% 0 0);\n--color-pink-300: oklch(82.3% 0.12 346.018);\n--color-emerald-200: oklch(90.5% 0.093 164.15);\n--color-blue-400: oklch(70.7% 0.165 254.624);\n--shadow-inner: inset 0 2px 4px 0 rgb(0 0 0 / 0.05);\n--ease-in: cubic-bezier(0.4, 0, 1, 1);\n--color-gray-300: oklch(87.2% 0.01 258.338);\n--color-emerald-600: oklch(59.6% 0.145 163.225);\n--color-indigo-600: oklch(51.1% 0.262 276.966);\n--color-red-400: oklch(70.4% 0.191 22.216);\n--color-stone-100: oklch(97% 0.001 106.424);\n--color-indigo-900: oklch(35.9% 0.144 278.697);\n--radius-sm: 0.25rem;\n--shadow-xs: 0 1px 2px 0 rgb(0 0 0 / 0.05);\n--color-pink-400: oklch(71.8% 0.202 349.761);\n--color-zinc-200: oklch(92% 0.004 286.32);\n--perspective-normal: 500px;\n--color-zinc-50: oklch(98.5% 0 0);\n--text-sm--line-height: calc(1.25 / 0.875);\n--color-cyan-500: oklch(71.5% 0.143 215.221);\n--text-9xl: 8rem;\n--radius-xl: 0.75rem;\n--color-cyan-300: oklch(86.5% 0.127 207.078);\n--color-gray-800: oklch(27.8% 0.033 256.848);\n--color-orange-400: oklch(75% 0.183 55.934);\n--radius-xs: 0.125rem;\n--perspective-near: 300px;\n--color-lime-400: oklch(84.1% 0.238 128.85);\n--text-sm: 0.875rem;\n--color-amber-100: oklch(96.2% 0.059 95.617);\n--animate-bounce: bounce 1s infinite;\n--color-sky-300: oklch(82.8% 0.111 230.318);\n--color-zinc-600: oklch(44.2% 0.017 285.786);\n--breakpoint-sm: 40rem;\n--color-gray-600: oklch(44.6% 0.03 256.802);\n--color-gray-100: oklch(96.7% 0.003 264.542);\n--container-5xl: 64rem;\n--text-xl: 1.25rem;\n--color-pink-700: oklch(52.5% 0.223 3.958);\n--color-purple-400: oklch(71.4% 0.203 305.504);\n--color-stone-500: oklch(55.3% 0.013 58.071);\n--text-base--line-height: calc(1.5 / 1);\n--color-violet-300: oklch(81.1% 0.111 293.571);\n--color-lime-50: oklch(98.6% 0.031 120.757);\n--color-sky-400: oklch(74.6% 0.16 232.661);\n--color-pink-900: oklch(40.8% 0.153 2.432);\n--color-red-200: oklch(88.5% 0.062 18.334);\n--color-lime-200: oklch(93.8% 0.127 124.321);\n--color-blue-50: oklch(97% 0.014 254.604);\n--color-stone-300: oklch(86.9% 0.005 56.366);\n--color-purple-700: oklch(49.6% 0.265 301.924);\n--shadow-2xs: 0 1px rgb(0 0 0 / 0.05);\n--text-lg--line-height: calc(1.75 / 1.125);\n--color-blue-300: oklch(80.9% 0.105 251.813);\n--ease-in-out: cubic-bezier(0.4, 0, 0.2, 1);\n--color-gray-200: oklch(92.8% 0.006 264.531);\n--color-sky-50: oklch(97.7% 0.013 236.62);\n--container-6xl: 72rem;\n--color-yellow-300: oklch(90.5% 0.182 98.111);\n--inset-shadow-sm: inset 0 2px 4px rgb(0 0 0 / 0.05);\n--color-pink-600: oklch(59.2% 0.249 0.584);\n--color-green-50: oklch(98.2% 0.018 155.826);\n--radius-2xl: 1rem;\n--container-2xs: 18rem;\n--color-orange-200: oklch(90.1% 0.076 70.697);\n--color-red-800: oklch(44.4% 0.177 26.899);\n--container-md: 28rem;\n--inset-shadow-xs: inset 0 1px 1px rgb(0 0 0 / 0.05);\n--color-amber-300: oklch(87.9% 0.169 91.605);\n--blur: 8px;\n--color-slate-400: oklch(70.4% 0.04 256.788);\n--color-violet-950: oklch(28.3% 0.141 291.089);\n--color-gray-900: oklch(21% 0.034 264.665);\n--font-weight-black: 900;\n--color-stone-950: oklch(14.7% 0.004 49.25);\n--color-emerald-400: oklch(76.5% 0.177 163.223);\n--color-zinc-300: oklch(87.1% 0.006 286.286);\n--container-xl: 36rem;\n--color-teal-400: oklch(77.7% 0.152 181.912);\n--color-cyan-50: oklch(98.4% 0.019 200.873);\n--color-neutral-700: oklch(37.1% 0 0);\n--color-rose-900: oklch(41% 0.159 10.272);\n--color-indigo-300: oklch(78.5% 0.115 274.713);\n--color-rose-700: oklch(51.4% 0.222 16.935);\n--color-green-300: oklch(87.1% 0.15 154.449);\n--color-zinc-700: oklch(37% 0.013 285.805);\n--text-4xl--line-height: calc(2.5 / 2.25);\n--color-amber-900: oklch(41.4% 0.112 45.904);\n--default-transition-duration: 150ms;\n--color-slate-800: oklch(27.9% 0.041 260.031);\n--color-orange-700: oklch(55.3% 0.195 38.402);\n--color-purple-100: oklch(94.6% 0.033 307.174);\n--text-2xl: 1.5rem;\n--color-red-900: oklch(39.6% 0.141 25.723);\n--color-teal-950: oklch(27.7% 0.046 192.524);\n--color-orange-500: oklch(70.5% 0.213 47.604);\n--color-lime-500: oklch(76.8% 0.233 130.85);\n--color-cyan-600: oklch(60.9% 0.126 221.723);\n--container-3xs: 16rem;\n--animate-ping: ping 1s cubic-bezier(0, 0, 0.2, 1) infinite;\n--tracking-normal: 0em;\n--color-green-900: oklch(39.3% 0.095 152.535);\n--color-neutral-950: oklch(14.5% 0 0);\n--text-6xl--line-height: 1;\n--color-amber-950: oklch(27.9% 0.077 45.635);\n--font-weight-medium: 500;\n--color-yellow-200: oklch(94.5% 0.129 101.54);\n--color-violet-100: oklch(94.3% 0.029 294.588);\n--perspective-midrange: 800px;\n--color-indigo-950: oklch(25.7% 0.09 281.288);\n--default-font-family: var(--font-sans, initial);\n--text-lg: 1.125rem;\n--color-indigo-700: oklch(45.7% 0.24 277.023);\n--color-emerald-800: oklch(43.2% 0.095 166.913);\n--blur-sm: 8px;\n--color-amber-700: oklch(55.5% 0.163 48.998);\n--color-neutral-900: oklch(20.5% 0 0);\n--color-neutral-100: oklch(97% 0 0);\n--default-mono-font-family: var(--font-mono, initial);\n--color-blue-600: oklch(54.6% 0.245 262.881);\n--default-mono-font-feature-settings: var(--font-mono--font-feature-settings, initial);\n--container-sm: 24rem;\n--color-teal-900: oklch(38.6% 0.063 188.416);\n--color-gray-400: oklch(70.7% 0.022 261.325);\n--color-indigo-50: oklch(96.2% 0.018 272.314);\n--color-lime-800: oklch(45.3% 0.124 130.933);\n--tracking-widest: 0.1em;\n--color-rose-400: oklch(71.2% 0.194 13.428);\n--color-violet-50: oklch(96.9% 0.016 293.756);\n--color-teal-700: oklch(51.1% 0.096 186.391);\n--color-stone-800: oklch(26.8% 0.007 34.298);\n--color-rose-950: oklch(27.1% 0.105 12.094);\n--color-sky-900: oklch(39.1% 0.09 240.876);\n--color-teal-500: oklch(70.4% 0.14 182.503);\n--color-cyan-700: oklch(52% 0.105 223.128);\n--color-amber-600: oklch(66.6% 0.179 58.318);\n--color-emerald-300: oklch(84.5% 0.143 164.978);\n--text-shadow-md: 0px 1px 1px rgb(0 0 0 / 0.1), 0px 1px 2px rgb(0 0 0 / 0.1), 0px 2px 4px rgb(0 0 0 / 0.1);\n--shadow-md: 0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1);\n--drop-shadow-md: 0 3px 3px rgb(0 0 0 / 0.12);\n--color-orange-800: oklch(47% 0.157 37.304);\n--text-shadow-xs: 0px 1px 1px rgb(0 0 0 / 0.2);\n--color-green-600: oklch(62.7% 0.194 149.214);\n--color-zinc-500: oklch(55.2% 0.016 285.938);\n--color-lime-300: oklch(89.7% 0.196 126.665);\n--text-7xl: 4.5rem;\n--color-lime-100: oklch(96.7% 0.067 122.328);\n--color-emerald-50: oklch(97.9% 0.021 166.113);\n--color-slate-700: oklch(37.2% 0.044 257.287);\n--color-lime-600: oklch(64.8% 0.2 131.684);\n--color-zinc-100: oklch(96.7% 0.001 286.375);\n--text-base: 1rem;\n--container-7xl: 80rem;\n--color-stone-700: oklch(37.4% 0.01 67.558);\n--leading-snug: 1.375;\n--color-violet-900: oklch(38% 0.189 293.745);\n--color-zinc-950: oklch(14.1% 0.005 285.823);\n--color-amber-200: oklch(92.4% 0.12 95.746);\n--tracking-tighter: -0.05em;\n--color-teal-200: oklch(91% 0.096 180.426);\n--color-yellow-600: oklch(68.1% 0.162 75.834);\n--color-purple-800: oklch(43.8% 0.218 303.724);\n--leading-relaxed: 1.625;\n--color-green-400: oklch(79.2% 0.209 151.711);\n--font-weight-normal: 400;\n--radius-md: 0.375rem;\n--color-blue-900: oklch(37.9% 0.146 265.522);\n--default-transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1);\n--color-purple-300: oklch(82.7% 0.119 306.383);\n--text-3xl: 1.875rem;\n--color-emerald-100: oklch(95% 0.052 163.051);\n--color-green-200: oklch(92.5% 0.084 155.995);\n--radius-4xl: 2rem;\n--color-gray-700: oklch(37.3% 0.034 259.733);\n--drop-shadow-xs: 0 1px 1px rgb(0 0 0 / 0.05);\n--radius-3xl: 1.5rem;\n--color-amber-800: oklch(47.3% 0.137 46.201);\n--color-fuchsia-950: oklch(29.3% 0.136 325.661);\n--color-purple-950: oklch(29.1% 0.149 302.717);\n--shadow-2xl: 0 25px 50px -12px rgb(0 0 0 / 0.25);\n--color-stone-50: oklch(98.5% 0.001 106.423);\n--color-sky-950: oklch(29.3% 0.066 243.157);\n--text-xl--line-height: calc(1.75 / 1.25);\n--text-6xl: 3.75rem;\n--color-emerald-950: oklch(26.2% 0.051 172.552);\n--color-pink-50: oklch(97.1% 0.014 343.198);\n--color-cyan-100: oklch(95.6% 0.045 203.388);\n--blur-2xl: 40px;\n--color-red-50: oklch(97.1% 0.013 17.38);\n--color-purple-200: oklch(90.2% 0.063 306.703);\n--color-gray-500: oklch(55.1% 0.027 264.364);\n--color-blue-200: oklch(88.2% 0.059 254.128);\n--color-blue-100: oklch(93.2% 0.032 255.585);\n--breakpoint-lg: 64rem;\n--color-rose-600: oklch(58.6% 0.253 17.585);\n--color-slate-500: oklch(55.4% 0.046 257.417);\n--color-fuchsia-600: oklch(59.1% 0.293 322.896);\n--color-violet-400: oklch(70.2% 0.183 293.541);\n--font-weight-extralight: 200;\n--color-stone-200: oklch(92.3% 0.003 48.717);\n--font-mono: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;\n--color-green-700: oklch(52.7% 0.154 150.069);\n--text-5xl: 3rem;\n--color-sky-800: oklch(44.3% 0.11 240.79);\n--color-stone-900: oklch(21.6% 0.006 56.043);\n--color-zinc-900: oklch(21% 0.006 285.885);\n--color-yellow-400: oklch(85.2% 0.199 91.936);\n--font-weight-semibold: 600;\n--color-teal-300: oklch(85.5% 0.138 181.071);\n--color-emerald-900: oklch(37.8% 0.077 168.94);\n--color-slate-950: oklch(12.9% 0.042 264.695);\n--font-sans: ui-sans-serif, system-ui, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';\n--container-4xl: 56rem;\n--color-stone-600: oklch(44.4% 0.011 73.639);\n--color-amber-400: oklch(82.8% 0.189 84.429);\n--color-purple-900: oklch(38.1% 0.176 304.987);\n--drop-shadow: 0 1px 2px rgb(0 0 0 / 0.1), 0 1px 1px rgb(0 0 0 / 0.06);\n--container-xs: 20rem;\n--color-neutral-400: oklch(70.8% 0 0);\n--color-red-100: oklch(93.6% 0.032 17.717);\n--color-gray-50: oklch(98.5% 0.002 247.839);\n--color-slate-200: oklch(92.9% 0.013 255.508);\n--font-weight-bold: 700;\n--color-violet-600: oklch(54.1% 0.281 293.009);\n--container-3xl: 48rem;\n--color-lime-700: oklch(53.2% 0.157 131.589);\n--color-stone-400: oklch(70.9% 0.01 56.259);\n--color-sky-700: oklch(50% 0.134 242.749);\n--blur-md: 12px;\n--color-slate-900: oklch(20.8% 0.042 265.755);\n--color-white: #fff;\n--color-cyan-400: oklch(78.9% 0.154 211.53);\n--color-gray-950: oklch(13% 0.028 261.692);\n--color-cyan-800: oklch(45% 0.085 224.283);\n--color-blue-700: oklch(48.8% 0.243 264.376);\n--font-serif: ui-serif, Georgia, Cambria, 'Times New Roman', Times, serif;\n--color-fuchsia-900: oklch(40.1% 0.17 325.612);\n--color-violet-700: oklch(49.1% 0.27 292.581);\n--color-violet-500: oklch(60.6% 0.25 292.717);\n--color-pink-500: oklch(65.6% 0.241 354.308);\n--color-yellow-100: oklch(97.3% 0.071 103.193);\n--color-zinc-400: oklch(70.5% 0.015 286.067);\n--color-teal-100: oklch(95.3% 0.051 180.801);\n--color-fuchsia-100: oklch(95.2% 0.037 318.852);\n--color-cyan-950: oklch(30.2% 0.056 229.695);\n--color-rose-50: oklch(96.9% 0.015 12.422);\n--color-teal-800: oklch(43.7% 0.078 188.216);\n--color-neutral-600: oklch(43.9% 0 0);\n--color-violet-800: oklch(43.2% 0.232 292.759);\n--color-zinc-800: oklch(27.4% 0.006 286.033);\n--color-rose-100: oklch(94.1% 0.03 12.58);\n--perspective-distant: 1200px;\n--color-rose-800: oklch(45.5% 0.188 13.697);\n--color-red-600: oklch(57.7% 0.245 27.325);\n--color-pink-800: oklch(45.9% 0.187 3.815);\n--color-yellow-800: oklch(47.6% 0.114 61.907);\n--color-cyan-900: oklch(39.8% 0.07 227.392);\n--color-rose-200: oklch(89.2% 0.058 10.001);\n--text-shadow-lg: 0px 1px 2px rgb(0 0 0 / 0.1), 0px 3px 2px rgb(0 0 0 / 0.1), 0px 4px 8px rgb(0 0 0 / 0.1);\n--text-7xl--line-height: 1;\n--spacing: 0.25rem;\n--color-orange-300: oklch(83.7% 0.128 66.29);\n--color-neutral-300: oklch(87% 0 0);\n--leading-normal: 1.5;\n--color-indigo-400: oklch(67.3% 0.182 276.935);\n--text-4xl: 2.25rem;\n--aspect-video: 16 / 9;\n--color-orange-900: oklch(40.8% 0.123 38.172);\n--color-violet-200: oklch(89.4% 0.057 293.283);\n--tracking-tight: -0.025em;\n--color-amber-500: oklch(76.9% 0.188 70.08);\n--color-fuchsia-700: oklch(51.8% 0.253 323.949);\n--color-purple-600: oklch(55.8% 0.288 302.321);\n--color-green-100: oklch(96.2% 0.044 156.743);\n--color-blue-800: oklch(42.4% 0.199 265.638);\n--color-emerald-700: oklch(50.8% 0.118 165.612);\n--color-yellow-500: oklch(79.5% 0.184 86.047);\n--color-indigo-500: oklch(58.5% 0.233 277.117);\n--ease-out: cubic-bezier(0, 0, 0.2, 1);\n--text-2xl--line-height: calc(2 / 1.5);\n--color-rose-500: oklch(64.5% 0.246 16.439);\n--text-xs: 0.75rem;\n--blur-xl: 24px;\n--leading-tight: 1.25;\n--color-orange-950: oklch(26.6% 0.079 36.259);\n--container-lg: 32rem;\n--color-rose-300: oklch(81% 0.117 11.638);\n--color-yellow-900: oklch(42.1% 0.095 57.708);\n--font-weight-thin: 100;\n--color-indigo-200: oklch(87% 0.065 274.039);\n--color-neutral-800: oklch(26.9% 0 0);\n--tracking-wider: 0.05em;\n--color-lime-950: oklch(27.4% 0.072 132.109);\n--color-orange-100: oklch(95.4% 0.038 75.164);\n--leading-loose: 2;\n--shadow-sm: 0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1);\n--color-slate-600: oklch(44.6% 0.043 257.281);\n--drop-shadow-2xl: 0 25px 25px rgb(0 0 0 / 0.15);\n--breakpoint-2xl: 96rem;\n--color-fuchsia-800: oklch(45.2% 0.211 324.591);\n--animate-spin: spin 1s linear infinite;\n--color-teal-600: oklch(60% 0.118 184.704);\n--color-pink-950: oklch(28.4% 0.109 3.907);\n--text-3xl--line-height: calc(2.25 / 1.875);\n--color-pink-100: oklch(94.8% 0.028 342.258);\n--color-amber-50: oklch(98.7% 0.022 95.277);\n}\n@keyframes spin {\n    to {\n      transform: rotate(360deg);\n    }\n  }\n@keyframes pulse {\n    50% {\n      opacity: 0.5;\n    }\n  }\n@keyframes ping {\n    75%,\n    100% {\n      transform: scale(2);\n      opacity: 0;\n    }\n  }\n@keyframes bounce {\n    0%,\n    100% {\n      transform: translateY(-25%);\n      animation-timing-function: cubic-bezier(0.8, 0, 1, 1);\n    }\n\n    50% {\n      transform: none;\n      animation-timing-function: cubic-bezier(0, 0, 0.2, 1);\n    }\n  }\n\n"