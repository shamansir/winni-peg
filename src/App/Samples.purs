module App.Samples where


type SampleName = String


type Sample =
    { name :: SampleName
    , grammar :: String
    , input :: String
    }


samples =
    [
        --- PLAIN ---
        { name : "plain"
        , grammar : """main :- repSep(., "")."""
        , input : """foobar"""
        }
    ,
        --- FP ---
        { name : "fp"
        , grammar : """main :- expr.

expr :- (funcCall | lambda | letExpr | varExpr | stringLit | intLit | placeholder).

funcCall :- [varExpr, "(", expr, ")"].
parenExpr :- ["(", expr, ")"].
lambda :- ["(", lambdaParam ,")", ws, ":", ws, type, ws, "=>", ws, expr].
lambdaParam :- [ident, ws, ":", ws, type].
letExpr :- [letKW, ws, ident, ws, "=", ws, expr, ws, inKW, ws, expr].
varExpr :- ident.
stringLit :- ["\"", repSep(stringChar, ""), "\""].
intLit :- [num, repSep(num, "")].

type :- ident.

alpha :- ([a-z] | [A-Z] | "_").
num :- [0-9].
alphaNum :- (alpha | num).
ident :- [alpha, repSep((alphaNum | "."), "")].
ws :- repSep((" "|"\n"), "").
placeholder :- "???".
stringChar :- (^'"' | ['\\', '"']).

letKW :- "let".
inKW :- "in".
"""
        , input : """let x = 2 in plus2(x)"""
        }
    ,
        --- BLOCKS ---
        { name : "blocks"
        , grammar : """main :- repSep(block, ws).
block :- [label, ws, blockBody].
blockBody :- ["{", ws, repSep(instr,[";", ws]), ws, "}"].
label :- ident.
instr :- (valueInstr | gotoInstr | forkToInstr).
forkToInstr :- [forkToKW, ws, label].
valueInstr :- [([ident, ws, "=", ws] | ""), rvalue].
rvalue :- (call | const).
call :- [ident, (params | "")].
gotoInstr :- [gotoKW, ws, (label | Placeholder), ([ws, ifClause] | "")].
ifClause :- [ifKW, ws, ident].
params :- ["(", repSep((ident | Placeholder), [",", ws]), ")"].
const :- (string | int | editorVar).
editorVar :- ["<<", int, ">>"].

gotoKW :- "goto".
forkToKW :- "forkTo".
ifKW :- "if".

int :- [[0-9], repSep([0-9], "")].
Placeholder :- "???".
ws :- repSep([spaces, (comment | "")], "\n").
spaces :- repSep(" ", "").
string :- ["\"", repSep(stringChar, ""), "\""].
stringChar :- (^'"' | ['\\', '"']).
alpha :- ([a-z] | [A-Z] | "_").
num :- [0-9].
alphaNum :- (alpha | num).
ident :- [alpha, repSep((alphaNum | "."), "")].
comment :- ["//", repSep(commentChar, "")].
commentChar :- ^'\n'.
"""
        , input : """main {
  lock = alloc.newLock();
  block.acquireLock(lock);
  forkTo afterFork;
  t = 25;
  block.sleep(t);
  block.releaseLock(lock);
  goto end;
}
afterFork {
  block.acquireLock(lock);
  x5 = 10;
}
end {}"""
        }
    ,
        --- CONTRACTS ---
        { name : "contracts"
        , grammar : """main :- repSep(stmt, ws).

stmt :- [(contractStmt | paymentStmt), "."].

contractStmt :- [contractKW, funcArgs].
paymentStmt :- [paymentKW, funcArgs].

expr :- (funcCall | lambda | varExpr | stringLit | intLit | placeholder).

funcCall :- [varExpr, funcArgs].
funcArgs :- ["(", ws, repSep(funcArg, commaWS), ws, ")"].
funcArg :- [([ident, ":"] | ""), expr].

parenExpr :- ["(", expr, ")"].
lambda :- ["(", lambdaParam ,")", ws, ":", ws, type, ws, "=>", ws, expr].
lambdaParam :- [ident, ws, ":", ws, type].
varExpr :- ident.
stringLit :- ["\"", stringContents, "\""].
stringContents :- repSep(stringChar, "").
intLit :- [num, repSep(num, "")].

type :- ident.

alpha :- ([a-z] | [A-Z] | "_").
num :- [0-9].
alphaNum :- (alpha | num).
ident :- [alpha, repSep((alphaNum | "."), "")].
ws :- repSep((" "|"\n"), "").
placeholder :- "???".
stringChar :- (^'"' | ['\\', '"']).
commaWS :- [",", ws].

contractKW :- "contract".
paymentKW :- "payment".
"""
        , input : """contract(
  id:1,
  contract:and(
    pay(from:"Alice", to:"Bob", amount:100),
    delay(
      duration:30,
      contract:pay(from:"Bob", to:"Alice", amount:110)
    )
  )
).

payment(id:1, contractID:1, from:"Alice", to:"Bob", amount:100, time:0).
payment(id:2, contractID:1, from:"Bob", to:"Alice", amount:110, time:30).
"""
        }
    ,
        --- DATALOG ---
        { name : "datalog"
        , grammar : """main :- [ws, repSep(statement, ws), ws].
statement :- (rule | fact | query | deleteFact | tableDecl | loadStmt).
comment :- ["#", repSep(commentChar, "")].
tableDecl :- [tableKW, ws, name:ident].
loadStmt :- [loadKW, ws, path].

query :- [record, "?"].
fact :- [record, "."].
deleteFact :- ["-", record, "."].

rule :- [record, ws, ":-", ws, repSep(disjunct, [ws, "|", ws]), "."].
disjunct :- repSep(conjunct, [ws, "&", ws]).
conjunct :- (record | comparison | arithmetic | negation | aggregation | placeholder).
negation :- ["!", record].
aggregation :- [aggregation:ident, "[", repSep(var, commaSpace), ":", ws, record, "]"].

comparison :- [left:term, ws, comparisonOp, ws, right:term].
comparisonOp :- ("<=" | ">=" | ">" | "<" | "=" | "!=").

arithmetic :- (assignmentOnLeft | assignmentOnRight).
assignmentOnRight :- [left:term, ws, arithmeticOp, ws, right:term, ws, "=", ws, result:term].
assignmentOnLeft :- [result:term, ws, "=", ws, left:term, ws, arithmeticOp, ws, right:term].
arithmeticOp :- ("+" | "*" | "-").

term :- (record | int | var | string | bool | array | dict | placeholder).
var :- [[A-Z], repSep(([A-Z]|alphaNum), "")].
record :- [ident, "{", ws, recordAttrs, ws, "}"].
dict :- ["{", ws, repSep(dictKeyValue, commaSpace), ws, "}"].
dictKeyValue :- [key:string, ":", ws, value:term].
recordAttrs :- repSep((recordKeyValue | placeholder), commaSpace).
recordKeyValue :- [ident, ":", ws, term].
int :- [("-" | ""), first:num, repSep(num, "")].
bool :- ("true" | "false").
array :- ["[", ws, repSep(term, commaSpace), ws, "]"].

tableKW :- ".table".
loadKW :- ".load".

ident :- [alpha, repSep((alphaNum | "."), "")].
string :- ["\"", repSep(stringChar, ""), "\""].
stringChar :- (^'"' | ['\\', '"']).
alpha :- ([a-z] | [A-Z] | "_").
num :- [0-9].
alphaNum :- (alpha | num).
ws :- repSep([spaces, (comment | "")], "\n").
spaces :- repSep(" ", "").
placeholder :- "???".
commaSpace :- [",", ws].
path :- repSep(pathSegment, "/").
pathSegment :- repSep(([a-z]|[A-Z]|[0-9]|'_'|'-'|'.'), "").

commentChar :- ^'\n'.
"""
        , input : """.table ast.string
.table ast.ident
.table ast.var
.table ast.int

hl.Segment{type: T, span: S, highlight: HH} :-
  hl.ident{type: T, span: S, highlight: H} |
  ??? |
  hl.int{???, span: S, highlightt: H} |
  # this is a comment inside a rule
  hl.string{type: T, span: S, highlight: ???}.

hl.ident{type: "ident", span: S, highlight: false} :-
  ast.ident{span: S}.
# this is a rule for vars
hl.var{type: "var", span: S, highlight: false} :-
  ast.var{span: S}.
hl.int{type: "int", span: S, highlight: false} :-
  ast.int{span: S}.
hl.string{type: "string", span: S, highlight: false} :-
  ast.string{span: S}.
batteryInputCurrent{id: B, time: T, current: L} :-
  sum[C: batteryInputCurrentFrom{id: B, time: T, current: C}].
"""
        }
    ,
        --- DATALOG 2 ---
        { name : "datalog 2"
        , grammar : """main :- [ws, repSep(declaration, ws), ws].
declaration :- (rule | tableDecl | import | fact).
comment :- ["#", repSep(commentChar, "")].
import :- [importKW, ws, path].

# TODO: probably shouldn't allow qualifiers here
tableDecl :- [
    tableKW, ws, name:qualifier, ws, "{", ws,
    repSep(tableAttr, commaSpace), ws,
    "}"
].
tableAttr :- [ident, (refSpec | "")].
refSpec :- [":", ws, (outRef | inRef)].
outRef :- [outRefKW, "(", table:qualifier, ":", col:ident, ")"].
inRef :- [inRefKW, "(", table:qualifier, ":", col:ident, ")"].

inRefKW :- "inRef".
outRefKW :- "outRef".

query :- [record, "?"].
fact :- [record, "."].
deleteFact :- ["-", record, "."].

rule :- [
    defKW, ws, record, ws, "{", ws,
    repSep(disjunct, [ws, "|", ws]), ws,
    "}"
].
disjunct :- repSep(conjunct, [ws, "&", ws]).
conjunct :- (nested | record | comparison | arithmetic | negation | aggregation | placeholder).
negation :- ["!", record].
aggregation :- [aggregation:ident, "[", repSep(var, commaSpace), ":", ws, record, "]"].

comparison :- [left:term, ws, comparisonOp, ws, right:term].
comparisonOp :- ("<=" | ">=" | ">" | "<" | "=" | "!=").

nested :- [qualifier, ws, "{", ws, repSep(nestedAttr, commaSpace), ws, "}"].
nestedAttr :- (normalAttr | nested).
normalAttr :- [ident, ":", ws, term].

arithmetic :- (assignmentOnLeft | assignmentOnRight).
assignmentOnRight :- [left:term, ws, arithmeticOp, ws, right:term, ws, "=", ws, result:term].
assignmentOnLeft :- [result:term, ws, "=", ws, left:term, ws, arithmeticOp, ws, right:term].
arithmeticOp :- ("+" | "*" | "-").

term :- (record | int | var | string | bool | array | dict | placeholder).
var :- [[A-Z], repSep(([A-Z]|alphaNum), "")].
record :- [qualifier, "{", ws, recordAttrs, ws, "}"].
dict :- ["{", ws, repSep(dictKeyValue, commaSpace), ws, "}"].
dictKeyValue :- [key:string, ":", ws, value:term].
recordAttrs :- repSep((recordKeyValue | placeholder), commaSpace).
recordKeyValue :- [ident, ":", ws, term].
int :- [("-" | ""), first:num, repSep(num, "")].
bool :- ("true" | "false").
array :- ["[", ws, repSep(term, commaSpace), ws, "]"].

tableKW :- "table".
importKW :- "import".
defKW :- "def".

qualifier :- repSep(ident, ".").
ident :- [alpha, repSep(alphaNum, "")].
string :- ["\"", repSep(stringChar, ""), "\""].
stringChar :- (^'"' | ['\\', '"']).
alpha :- ([a-z] | [A-Z] | "_").
num :- [0-9].
alphaNum :- (alpha | num).
ws :- repSep([spaces, (comment | "")], "\n").
spaces :- repSep(" ", "").
placeholder :- "???".
commaSpace :- [",", ws].
path :- ["\"", repSep(pathSegment, "/"), "\""].
pathSegment :- repSep(([a-z]|[A-Z]|[0-9]|'_'|'-'|'.'), "").

commentChar :- ^'\n'.
"""
        , input : """import "ast"

table constraint {
  id,
  parentID,
  record: inRef(ast.record:parentID),
  constraintComparison: inRef(constraintComparison:parentID)
}
table ast.record {
  id,
  parentID,
  ident: inRef(ident:parentID)
}
table constraintComparison {
  parentID,
  scalarExpr: inRef(scalarExpr:parentID)
}
table scalarExpr {
  parentID,
  scalarTerm: inRef(scalarTerm:parentID)
}
table scalarTerm {
  parentID,
  term: inRef(term:parentID)
}
table term {
  parentID,
  var: inRef(var:parentID)
}
table var {
  parentID,
  text,
  span
}
table ident {
  parentID,
  text,
  span
}

def varInComparison{scope: I, name: N, span: S} {
  constraint{
    id: Foo,
    record {
      id: Bar,
      ident {
        text: I
      }
    },
    constraintComparison {
      scalarExpr {
        scalarTerm {
          term {
            var{text: N, span: S}
          }
        }
      }
    }
  }
}
"""
        }
    ,
        --- GRAMMAR ---
        { name : "grammar"
        , grammar : """main :- repSep((ruleDefn | comment), ws).

comment :- ["#", repSep(commentChar, "")].
ruleDefn :- [ident, ws, ":-", ws, rule, "."].

rule :- (seq | choice | ref | text | charRule | repSep | placeholder).

seq :- ["[", ws, repSep(rule, [ws, ",", ws]), ws, "]"].
choice :- ["(", ws, repSep(rule, [ws, "|", ws]), ws, ")"].
ref :- [([captureName, ":"] | ""), ruleName].
captureName :- ident.
ruleName :- ident.
text :- ["\"", repSep(stringChar, ""), "\""].
repSep :- [repSepKW, "(", rep:rule, commaSpace, sep:rule, ")"].
repSepKW :- "repSep".

charRule :- (charRange | notChar | singleChar | anyChar).
charRange :- ["[", from:alphaNum, "-", to:alphaNum, "]"].
notChar :- ['^', charRule].
singleChar :- ["'", (['\\', 'n'] | ['\\', '\\'] | .), "'"].
anyChar :- ".".

ident :- repSep(alpha, "").
ws :- repSep((" "|"\n"), "").
commaSpace :- [",", ws].
placeholder :- "???".

alphaNum :- (alpha | num).
alpha :- ([a-z] | [A-Z]).
num :- [0-9].
stringChar :- (^'"' | ['\\', '"']).
commentChar :- ^'\n'.
"""
        , input : """main :- value.
value :- (object | array | int | string | null).
int :- [[0-9], repSep([0-9], "")].
object :- ["{", repSep(keyValue, ","), "}"].
keyValue :- [string, ":", value].
string :- ["\"", repSep([a-z], ""), "\""].
array :- ["[", repSep(value, ","), "]"].
null :- "null".
"""
        }
    ,
        --- GRAMMAR 2---
        { name : "grammar-2"
        , grammar : """main :- repSep((ruleDefn | comment), ws).

comment :- ["#", repSep(commentChar, "")].
ruleDefn :- [ident, ws, ":-", ws, rule, "."].

rule :- (seq | choice | charRule | text | repSep | placeholder | ref).

seq :- ["[", ws, repSep(rule, [ws, ",", ws]), ws, "]"].
choice :- ["(", ws, repSep(rule, [ws, "|", ws]), ws, ")"].
ref :- [([captureName, ":"] | ""), ruleName].
captureName :- ident.
ruleName :- ident.
text :- ["\"", repSep(stringChar, ""), "\""].
repSep :- [repSepKW, "(", rep:rule, commaSpace, sep:rule, ")"].
repSepKW :- "repSep".

charRule :- (charRange | notChar | singleChar | anyChar).
charRange :- ["[", from:alphaNum, "-", to:alphaNum, "]"].
notChar :- ['^', charRule].
singleChar :- ["'", (['\\', 'n'] | ['\\', '\\'] | .), "'"].
anyChar :- ".".

ident :- repSep(alpha, "").
ws :- repSep((" "|"\n"), "").
commaSpace :- [",", ws].
placeholder :- "???".

alphaNum :- (alpha | num).
alpha :- ([a-z] | [A-Z]).
num :- [0-9].
stringChar :- (['\\', '"'] | ^'"').
#stringChar :- (^'"' | ['\\', '"']).
commentChar :- ^'\n'.
"""
        , input : """main :- value.
value :- (object | array | int | string | null).
int :- [[0-9], repSep([0-9], "")].
object :- ["{", repSep(keyValue, ","), "}"].
keyValue :- [string, ":", value].
string :- ["\"", repSep([a-z], ""), "\""].
array :- ["[", repSep(value, ","), "]"].
null :- "null".
"""
        }
    ,
        --- JSON ---
        { name : "json"
        , grammar : """main :- value.
value :- (object | array | int | string | null).
int :- [[0-9], repSep([0-9], "")].
object :- ["{", repSep(keyValue, ","), "}"].
keyValue :- [string, ":", value].
array :- ["[", repSep(value, ","), "]"].
null :- "null".
string :- ["\"", repSep(stringChar, ""), "\""].
stringChar :- (^'"' | ['\\', '"']).
"""
        , input : """{"foo":[1,2,3],"test":null}"""
        }
    ,
        --- MODELICA ---
        { name : "modelica"
        , grammar : """main :- model.
model :- [modelKW, ws, ident, params, ws, vars, ws, initKW, ws, init, ws, equationKW, ws, equations, ws, endKW].
params :- ["(", repSep(ident, commaSpace), ")"].

vars :- repSep(varDecl, ws).
init :- repSep(initEq, ws).
varDecl :- [var, ";"].
initEq :- [var, ws, "=", ws, expr, ";"].
expr :- (intLit | derivative | var).
var :- ident.
derivative :- [derKW, "(", ident, ")"].
equations :- repSep(equation, ws).
equation :- [expr, ws, "=", ws, expr, ";"].

alpha :- ([a-z] | [A-Z] | "_").
num :- [0-9].
intLit :- [[0-9], repSep([0-9], "")].
alphaNum :- (alpha | num).
ident :- [alpha, repSep((alphaNum | "."), "")].
ws :- repSep((" "|"\n"), "").
commaSpace :- [",", ws].
semicolonSpace :- [";", ws].
placeholder :- "???".
stringChar :- (^'"' | ['\\', '"']).

modelKW :- "model".
initKW :- "init".
equationKW :- "equation".
endKW :- "end".
derKW :- "der".
"""
        , input : """model FallingBall(initHeight)
  h;
  v;
init
  h = initHeight;
  v = 0;
equation
  v = der(h);
  der(v) = 9;
end"""
        }
    ,
        --- OPT ---
        { name : "opt"
        , grammar : """main :- repSep(declaration, ws).

declaration :- (
  varRelationDecl |
  inputRelationDecl |
  paramDecl |
  objective |
  constraint
).

varRelationDecl :- [varKW, ws, ident, ws, schemaSpec].
inputRelationDecl :- [inputKW, ws, ident, ws, schemaSpec].
paramDecl :- [
  paramKW, ws, ident, ws, "{", ws,
  repSep(paramLimit, commaSpace), ws,
  "}"
].
paramLimit :- [("min" | "max" | "init"), ":", ws, int].

schemaSpec :- ["{", ws, repSep(ident, [",", ws]), ws, "}"].

objective :- [
  senseKW, ws, "{", ws,
  conjuncts,
  ws, sepKW, ws,
  scalarExpr, ws,
  "}"
].

constraint :- [
  ruleKW, ws, record, ws, "{", ws,
  conjuncts, ws,
  sepKW, ws,
  constraintComparison, ws,
"}"].

constraintComparison :- [left:scalarExpr, ws, comparisonOp, ws, right:scalarExpr].

conjuncts :- repSep(clause, [ws, "&", ws]).
clause :- (record | comparison).

scalarExpr :- repSep(scalarTerm, [ws, "+", ws]).
scalarTerm :- [([coefficient:term, ws, "*", ws] | ""), term:term].
comparison :- [left:term, ws, comparisonOp, ws, right:term].
comparisonOp :- ("<=" | ">=" | ">" | "<" | "=" | "!=").

record :- [ident, "{", ws, recordAttrs, ws, "}"].
recordAttrs :- repSep((recordKeyValue | placeholder), commaSpace).
recordKeyValue :- [ident, ":", ws, term].

term :- (record | int | var | string | placeholder).
var :- [[A-Z], repSep(([A-Z]|alphaNum), "")].

senseKW :- ("minimize" | "maximize").
varKW :- "var".
inputKW :- "input".
ruleKW :- "rule".
sepKW :- "=>".
paramKW :- "param".

# TODO: comments

ident :- [alpha, repSep((alphaNum | "."), "")].
int :- [("-" | ""), first:num, repSep(num, "")].
string :- ["\"", repSep(stringChar, ""), "\""].
stringChar :- (^'"' | ['\\', '"']).
alpha :- ([a-z] | [A-Z] | "_").
num :- [0-9].
alphaNum :- (alpha | num).
ws :- repSep((" "|"\n"), "").
commaWS :- [",", ws].
placeholder :- "???".
commaSpace :- [",", ws].
"""
        , input : """var coalOutput {
  timestep,
  level
}

var natGasOutput {
  timestep,
  level
}

input loadLevel {
  timestep,
  level
}

param coalMax { min: 0, max: 100, init: 50 }
param coalRampRate { min: 0, max: 100, init: 50 }
param coalPrice { min: 0, max: 100, init: 50 }
param natGasPrice { min: 0, max: 100, init: 50 }

minimize {
  loadLevel{timestep: T} &
  coalOutput{level: CoalLevel, timestep: T} &
  natGasOutput{level: NatGasLevel, timestep: T} &
  coalPrice{price: CoalPrice} &
  natGasPrice{price: NatGasPrice}
  =>
  CoalPrice*CoalLevel + NatGasPrice*NatGasLevel
}

rule outputEqualsDemand{timestep: T} {
  loadLevel{level: LoadLevel, timestep: T} &
  coalOutput{level: CoalLevel, timestep: T} &
  natGasOutput{level: NatGasLevel, timestep: T}
  =>
  CoalLevel + NatGasLevel = LoadLevel
}

rule coalMax{timestep: T} {
  loadLevel{timestep: T} &
  coalOutput{level: CoalLevel, timestep: T} &
  coalMax{level: MaxLevel}
  =>
  CoalLevel <= MaxLevel
}

rule coalRampRate{timestep: T2} {
  loadLevel{timestep: T1} &
  loadLevel{timestep: T2} &
  coalOutput{level: L1, timestep: T1} &
  coalOutput{level: L2, timestep: T2} &
  base.add{a: T1, b: 1, res: T2} &
  coalRampRate{rate: RampRate}
  =>
  -1*L1 + L2 <= RampRate
}
"""
        }
    ,
        --- SQL ---
        { name : "sql"
        , grammar : """main :- repSep(statementSemicolon, ws).

statementSemicolon :- [statement, ";"].
statement :- (selectStmt | createTableStmt).

selectStmt :- [selectKW, ws, selection, ws, fromKW, ws, tableName].
selection :- repSep(columnName, commaWS).

createTableStmt :- [createKW, ws, tableKW, ws, tableName, ws, "(", ws, repSep(colSpec, commaWS), ws, ")"].

colSpec :- [columnName, ws, type, ws, (refClause | "")].
refClause :- [refKW, ws, tableName, ws, "(", columnName, ")"].

type :- ("INT" | "TEXT").

columnName :- (ident | placeholder).
tableName :- (ident | placeholder).

createKW :- "CREATE".
tableKW :- "TABLE".
selectKW :- "SELECT".
fromKW :- "FROM".
refKW :- "REFERENCES".

ident :- [alpha, repSep((alphaNum | "."), "")].
string :- ["\"", repSep(stringChar, ""), "\""].
stringChar :- (^'"' | ['\\', '"']).
alpha :- ([a-z] | [A-Z] | "_").
num :- [0-9].
alphaNum :- (alpha | num).
ws :- repSep((" "|"\n"), "").
commaWS :- [",", ws].
placeholder :- "???".
"""
        , input : """CREATE TABLE users (
  id INT,
  name TEXT
);

CREATE TABLE posts (
  id INT,
  user_id INT REFERENCES users (id),
  body TEXT
);

SELECT id FROM users;
SELECT id, ??? FROM posts;"""
        }
    ,
        --- TREE SQL ---
        { name : "tree-sql"
        , grammar : """main :- repSep(statement, ws).

statement :- [(selectStmt | createTableStmt), ";"].

selectStmt :- [(manyKW | oneKW), ws, tableName, ws, selection].
selection :- ["{", ws, repSep(selectionElement, commaWS), ws, "}"].
selectionElement :- [columnName, (subSelection | "")].
subSelection :- [":", ws, selectStmt].

createTableStmt :- [createKW, ws, tableKW, ws, tableName, ws, "(", ws, repSep(colSpec, commaWS), ws, ")"].

colSpec :- [columnName, ws, type, ws, (referencesClause | "")].

type :- ("INT" | "TEXT").
referencesClause :- [referencesKW, ws, tableName].

columnName :- (ident | placeholder).
tableName :- (ident | placeholder).

createKW :- "CREATE".
tableKW :- "TABLE".
manyKW :- "MANY".
oneKW :- "ONE".
referencesKW :- "REFERENCES".

ident :- [alpha, repSep((alphaNum | "."), "")].
string :- ["\"", repSep(stringChar, ""), "\""].
stringChar :- (^'"' | ['\\', '"']).
alpha :- ([a-z] | [A-Z] | "_").
num :- [0-9].
alphaNum :- (alpha | num).
ws :- repSep((" "|"\n"), "").
commaWS :- [",", ws].
placeholder :- "???".
"""
        , input : """CREATE TABLE users (
  id INT,
  name TEXT
);

CREATE TABLE posts (
  id INT,
  user_id INT REFERENCES users,
  body TEXT
);

MANY posts {
  id,
  body,
  user: ONE users {
    name,
    ???
  }
};
"""
        }
      ,
        -- ORG
      { name : "org"
      , grammar : """main :- [onls,kws,onls,repSep(heading,nls),repSep(paragraph,nls)].

# Keywords
kws :- repSep(kw,nls).
kw :- ["#+", kwname, ws, ":", ws, kwvalue].
kwname :- repSep(^':',"").
kwvalue :- repSep(^'\n',"").


# Headings
heading :- [hlevel, ws, headingContent].
hlevel :- [hmarker,repSep(hmarker, "")].
hmarker :- '*'.
headingContent :- repSep(^'\n',"").

paragraph :- repSep((bold|italic|word)," ").
word :- repSep(^' ',"").
bold :- ['*',repSep(^'*',""),'*'].
italic :- ['~',repSep(^'~',""),'~'].


nl :- '\n'.
nls :- [ws,nl,onls].
onls :- repSep([ws,nl],"").
ws :- repSep(' ',"").
"""
      , input : """#+title: The glories of Org
#+author: A. Org Author


* First Level Heading
** Second Level Heading
*** Third Level Heading
**** Fourth Level Heading


Test *bold* and ~italic~ inside paragraph. Also, *as several words*.
""" }
    ] :: Array Sample