package somepkg

type funk func()string

var Abc funk

func init(){
  Abc = func() string{ return "ABC" }
}
