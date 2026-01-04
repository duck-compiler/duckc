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

func isJsonWhitespace(r rune) bool {
	return r == '\r' || r == '\n' || r == '\t' || r == ' '
}

func scan_json_string(s string) (string, int, error) {
	e := errors.New("hallo")
	{
		{
			{
				r, _ := utf8.DecodeRuneInString(s[0:])
				if r != '"' {
					{
						return "", 0, e
					}
				}
			}
		}

		res := "\""

		i := 1

		success := false

		for w := 0; i < len(s); i += w {
			w = 0
			r, width := utf8.DecodeRuneInString(s[i:])

			if r == '\\' {
				if i < len(s)-1 {
					new_i := i + width
					r, width := utf8.DecodeRuneInString(s[new_i:])

					if r == '"' {
						res += "\""
						i = new_i
						w = width
						continue
					}
				} else {
					return "", 0, e
				}
			}

			res = res + string(r)

			if r == '"' {
				success = true
				break
			}

			w = width
		}

		if !success {
			return res, 0, e
		}

		e = json.Unmarshal([]byte(res), &res)

		if e != nil {
			return res, i, nil
		}
		return fmt.Sprintf("\"%s\"", res), i, e
	}
}

func object_to_string(obj map[string]string) string {
	res := ""
	needs_comma := false
	for k, v := range obj {
		if needs_comma {
			res += ","
		}

		res += fmt.Sprintf("\"%s\":%s", k, v)

		needs_comma = true
	}
	return fmt.Sprintf("{%s}", res)
}

func scan_json_struct_parts(s string) (map[string]string, int, error) {
	{
		res := make(map[string]string)
		_ = res

		e := errors.New("object scanning failed")

		{
			{
				r, _ := utf8.DecodeRuneInString(s[0:])
				if r != '{' {
					{
						return res, 0, e
					}
				}
			}
		}

		i := 1

		need_comma := false

	outer:
		for {
			if need_comma {
				for {
					r, width := utf8.DecodeRuneInString(s[i:])

					if r != ',' {
						if r == '}' {
							break outer
						}
						i += width
						continue
					}
					break
				}
			}

			for {
				r, width := utf8.DecodeRuneInString(s[i:])

				if r != '"' {
					if r == '}' {
						break outer
					}
					i += width
					continue
				}
				break
			}

			key, key_w, key_error := scan_json_string(s[i:])

			if key_error != nil {
				return res, 0, key_error
			}

			key = key[1:(len(key) - 1)]

			i += key_w + 1

			for {
				if i >= len(s) {
					return res, 0, key_error
				}
				r, width := utf8.DecodeRuneInString(s[i:])

				if r != ':' {
					i += width
					continue
				}
				break
			}

			i += 1

		value:
			for {
				if i >= len(s) {
					return res, 0, key_error
				}
				r, width := utf8.DecodeRuneInString(s[i:])

				if isJsonWhitespace(r) {
					i += width
					continue
				} else if r == ',' || r == ']' {
					return res, 0, e
				}

				if r == '"' {
					elem, elem_width, elem_err := scan_json_string(s[i:])
					if elem_err != nil {
						return res, 0, elem_err
					}
					i += elem_width + 1
					res[key] = elem
					break
				} else if r == '[' {
					elem, elem_width, elem_err := scan_json_array_parts(s[i:])
					if elem_err != nil {
						return res, 0, elem_err
					}
					i += elem_width + 1
					res[key] = fmt.Sprintf("[%s]", strings.Join(elem, ","))
					break
				} else if r == '{' {
					elem, elem_width, elem_err := scan_json_struct_parts(s[i:])
					if elem_err != nil {
						return res, 0, elem_err
					}
					i += elem_width + 1
					res[key] = object_to_string(elem)
					break
				} else {
					start_i := i
					for {
						r, width := utf8.DecodeRuneInString(s[i:])
						if r == ',' {
							res[key] = s[start_i:i]
							break value
						}

						if r == '}' {
							res[key] = s[start_i:i]
							break outer
						}

						i += width
					}
				}

			}

			need_comma = true

		}

		return res, i, nil
	}
}

func scan_json_array_parts(s string) ([]string, int, error) {
	{
		res := []string{}
		e := errors.New("array scanning failed")

		{
			{
				r, _ := utf8.DecodeRuneInString(s[0:])
				if r != '[' {
					{
						return res, 0, e
					}
				}
			}
		}

		i := 1

		current := ""

		pushPart := func() {
			{
				if len(current) > 0 {
					{
						res = append(res, current)
						current = ""
					}
				}
			}
		}

		success := false

		for w := 0; i < len(s); i += w {
			r, width := utf8.DecodeRuneInString(s[i:])

			if isJsonWhitespace(r) {
				w = width
				continue
			}

			if r == ']' {
				success = true
				pushPart()
				break
			}

			if r == ',' {
				pushPart()
				w = width
				continue
			}

			if r == '[' {
				res0, i0, err0 := scan_json_array_parts(s[i:])
				if err0 != nil {
					{
						return res, 0, err0
					}
				}
				current += fmt.Sprintf("[%s]", strings.Join(res0, ","))
				pushPart()
				w = i0 + 1
				continue
			}

			if r == '"' {
				res0, i0, err0 := scan_json_string(s[i:])
				if err0 != nil {
					{
						return res, 0, err0
					}
				}
				current += fmt.Sprintf("\"%s\"", res0)
				pushPart()
				w = i0 + 1
				continue
			}

			if r == '{' {
				res0, i0, err0 := scan_json_struct_parts(s[i:])
				if err0 != nil {
					{
						return res, 0, err0
					}
				}
				current += object_to_string(res0)
				pushPart()
				w = i0 + 1
				continue
			}

			current = current + string(r)

			w = width
		}

		if !success {
			return res, 0, e
		}

		return res, i, nil
	}
}
