
## Create your own DSL in Go

> by __Nathaniel Cook from InfluxData__ at Utah Go User Group, **Aug' 2017**

> [sample language code](https://github.com/nathanielc/jim/tree/master/dsl)

### Types

* Internal: implemented as a subset of existing language, like Chef DSL where you are writing Ruby even when writing DSL

* External: completely independent of any host language, like TICKScript


### Examples

* Go Templates, Rule Engines, TICKScript


### Example __home automation__ DSL

```
set masterbedroom/light 50
var bed_lights = get masterbedroom/light
set downstairs/light bed_lights

scene night_time {
  set */light off
  set */door locked
  set porch/light on

  when
    */door is unlocked
  wait 5m
    set $ locked
}

at 9:00 PM start night_time
at 8:00 AM stop  night_time
```

### Components of a DSL

```
func Lex(input string) <- chan Token

func Parse(tokens <-chan Token) (AST, error)

func Eval(AST) error
```


### Lexing

* consumes `runes` of text, produces tokens

```
func (l *lexer) lex() {
	for state := lexToken; state != nil {
		state = state(l)
	}
}
```

* Token type communicates between Lexer and Parser

#### State Function Loop

* is a type recursive function, returns an instance of same type

* `stateFn`; helps Lexer manage current state based on current keyword

```
type stateFn func(l *lexer) stateFn
```

* `emit`; need to be able to emit tokens to be utilized in state function

```
func (l *lexer) emit(t TokenType) {
	l.tokens <- Token{
		Position: l.position(),
		Type: t,
		Value: l.current(),
	}
	l.updatePositionCounters()
}

func (l *lexer) current() string {
	return l.input[l.start:l.position]
}

```


### Grammar

* need to write a grammar of production rules, so lexer can branch out

* example

```
digit          = "0".."9" .
ascii_letter   = "A".."Z" | "a".."z" .
letter         = ascii_letter | "_" .
word           = ( letter ) { letter | digit } .
time           = ( digit ) ":" (digit) ( "AM" | "PM" ) .

Program           = { ProgramStatement | BlockStatement } .
Block             = "{" { BlockStatement } "}" | BlockStatement .
ProgramStatement  = ScenetStatement .
BlockStatement    = SetStatement | GetStatement | VarStatement | AtStatement | WhenStatement .
SetStatement      = "set" PathMatch Value .
GetStatement      = "get" PathMatch .
VarStatement      = "var" word "=" GetStatement .
AtStatement       = "at" Time Action word .
WhenStatement     = "when" PathMatch "is" Value "wait" duration Block
ScenetStatement   = "scene" word Block .
Time              = { digit } ":" { digit } ( "AM" | "PM" )
Action            = ( "start" | "stop" )
PathMatch         = "$" | { ( word | "*" ) "/" } ( word | "*" ) .
```


### Building AST

* building AST nodes from production rules

```
// SetStatement  = "set" PathMatch Value .

type Node interface {
  Pos() Position
}
type SetStatementNode struct {
  Position
  DeviceMatch *PathMatchNode
  Value       *ValueNode
}
```

* parsing a basic `set`

```
func (p *parser) setStatement() *SetStatementNode {
	t := p.expect(TokenSet)
	pm := p.pathMatch()
	v := p.value()
	return &SetStatementNode{
		Position:    t.Pos,
		DeviceMatch: pm,
		Value:       v,
	}
}
```

* parsing a block which would be a collection of multiple fundamental constructs

```
func (p *parser) blockStatement() Node {
  switch p.peek().Type {
    case TokenSet:
      return p.setStatement()
    case TokenGet:
      return p.getStatement()
    case TokenVar:
      return p.varStatement()
    case TokenAt:
      return p.atStatement()
    case TokenWhen:
      return p.whenStatement()
    default:
      p.unexpected(p.next(), TokenSet, TokenVar, TokenAt, TokenWhen)
      return nil
  }
}
```

#### Syntax vs Semantics

* not all valid syntax is a valid program

> allow syntax that has no semantic meaning while parsing, validate the AST later to keep quick failures for obvious syntactical errors


### Eval

* given an AST perform actions in an order

* evaluator shall be managed separate from lexer/parser/AST modules, as evaluator would be different for different platforms/targets but initial modules would be same

* example eval implementation

```
func (e *Evaluator) eval(node dsl.Node) (Result, error) {
  switch n := node.(type) {
    case *dsl.ProgramNode:
      return e.evalNodeList(n.Statements)
    case *dsl.SetStatementNode:
      return e.evalSet(n)
    case *dsl.GetStatementNode:
      return e.evalGet(n)
    case *dsl.WhenStatementNode:
      return e.evalWhen(n)
    case *dsl.BlockNode:
      return e.evalNodeList(n.Statements)
    default:
      return nil, fmt.Errorf("unknown command %T", node)
  }
}
```

---
