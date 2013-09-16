package joinr

//joins the given strings
func Join(strs ...string) string {
  retVal := ""
  for _, s := range strs {
    retVal = retVal + string(s)
  }
  return retVal
}

//joins the given strings
func JoinArr(strs []string) string {
  retVal := ""
  for _, s := range strs {
    retVal = retVal + string(s)
  }
  return retVal
}
