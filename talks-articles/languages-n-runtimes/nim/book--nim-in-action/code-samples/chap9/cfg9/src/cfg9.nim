import macros


## proc newAppCfg9(): AppCfg9 =
##   new result
template constructor(ident: untyped): untyped =
  proc `new ident`(): `ident` =
    new result


proc createRefType(ident: NimIdent, identDefs: seq[NimNode]): NimNode =
  result = newTree(                           # StmtList
    nnkTypeSection,                           #   TypeSection
    newTree(                                  #
      nnkTypeDef,                             #     TypeDef
      newIdentNode(ident),                    #       Ident "AppCfg9"
      newEmptyNode(),                         #       Empty
      newTree(                                #
        nnkRefTy,                             #       RefTy
        newTree(                              #
          nnkObjectTy,                        #         ObjectTy
          newEmptyNode(),                     #           Empty
          newEmptyNode(),                     #           Empty
          newTree(                            #
            nnkRecList,                       #           RecList
            identDefs                         #           IdentDefs
          )
        )
      )
    )
  )


proc toIdentDefs(stmtList: NimNode): seq[NimNode] =
  expectKind(stmtList, nnkStmtList)
  result = @[]
  for child in stmtList:
    expectKind(child, nnkCall)    ## ensure input AST doesn't contain unexpected node kinds:q

    result.add(
      newIdentDefs(
        child[0],
        child[1][0]
      )
    )


## proc load*(cfg: AppCfg9, filename: string) =
##   var obj = parseFile(filename)
##   cfg.address = obj["address"].getStr
##   cfg.port = obj["port"].getNum.int



proc createLoadProc(typeName: NimIdent, identDefs: seq[NimNode]): NimNode =
  var cfgIdent = newIdentNode("cfg")
  var filenameIdent = newIdentNode("filename")
  var objIdent = newIdentNode("obj")

  var body = newStmtList()
  body.add quote do:
    var `objIdent` = parseFile(`filenameIdent`)

  for identDef in identDefs:
    let fieldNameIdent = identDef[0]
    let fieldName = $fieldNameIdent.ident
    case $identDef[1].ident
    of "string":
      body.add quote do:
        `cfgIdent`.`fieldNameIdent` = `objIdent`[`fieldName`].getStr
    of "int":
      body.add quote do:
        `cfgIdent`.`fieldNameIdent` = `objIdent`[`fieldName`].getInt().int
    else:
      doAssert(false, "Not Implemented")

  return newProc(newIdentNode("load"),
    [newEmptyNode(),
     newIdentDefs(cfgIdent, newIdentNode(typeName)),
     newIdentDefs(filenameIdent, newIdentNode("string"))],
    body)


macro config*(typeName: untyped, fields: untyped): untyped =
  result = newStmtList()
  let identDefs = toIdentDefs(fields)
  result.add createRefType(typeName.ident, identDefs)
  result.add getAst(constructor(typeName.ident))
  result.add createLoadProc(typeName.ident, identDefs)
  echo treeRepr(typeName)
  echo treeRepr(fields)
  echo treeRepr(result)
  echo repr(result)


## dumpTree:
##   type
##     AppCfg9 = ref object
##       address: string
##       port: int
##
##
## macro config(typeName: untyped, fields: untyped): untyped =
##   result = newStmtList()
##   echo treeRepr(typeName)
##   echo treeRepr(fields)
##
##
## config AppCfg9:
##   address: string
##   port: int
