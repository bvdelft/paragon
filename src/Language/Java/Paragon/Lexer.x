{
{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
    -fno-warn-unused-matches
    -fno-warn-name-shadowing
 #-}

-- | Paragon lexing module. Lexer is written using Alex.
module Language.Java.Paragon.Lexer
  (
    lexer
  , Token(..)
  , TokenWithSpan(..)
  ) where

import Data.List (intercalate)

import Language.Java.Paragon.SrcPos
}

%wrapper "posn"

$digit   = [0-9]
$nonzero = [1-9]
$octdig  = [0-7]
$hexdig  = [0-9A-Fa-f]

@lineterm = [\n\r] | \r\n

@tradcomment = \/\*(~[\*]|[\r\n]|(\*+(~[\*\/]|[\r\n])))*\*+\/
@linecomment = "//" .* @lineterm
@comment = @tradcomment | @linecomment

$javaLetter = [a-zA-Z\_\$]
$javaDigit = $digit
$javaLetterOrDigit = [a-zA-Z0-9\_\$]

tokens :-

  $white+  ;
  @comment ;

  -- Keywords
  abstract     { \p s -> TokWSpan KW_Abstract     (posn p s) }
  assert       { \p s -> TokWSpan KW_Assert       (posn p s) }
  boolean      { \p s -> TokWSpan KW_Boolean      (posn p s) }
  break        { \p s -> TokWSpan KW_Break        (posn p s) }
  byte         { \p s -> TokWSpan KW_Byte         (posn p s) }
  case         { \p s -> TokWSpan KW_Case         (posn p s) }
  catch        { \p s -> TokWSpan KW_Catch        (posn p s) }
  char         { \p s -> TokWSpan KW_Char         (posn p s) }
  class        { \p s -> TokWSpan KW_Class        (posn p s) }
  const        { \p s -> TokWSpan KW_Const        (posn p s) }
  continue     { \p s -> TokWSpan KW_Continue     (posn p s) }
  default      { \p s -> TokWSpan KW_Default      (posn p s) }
  do           { \p s -> TokWSpan KW_Do           (posn p s) }
  double       { \p s -> TokWSpan KW_Double       (posn p s) }
  else         { \p s -> TokWSpan KW_Else         (posn p s) }
  enum         { \p s -> TokWSpan KW_Enum         (posn p s) }
  extends      { \p s -> TokWSpan KW_Extends      (posn p s) }
  final        { \p s -> TokWSpan KW_Final        (posn p s) }
  finally      { \p s -> TokWSpan KW_Finally      (posn p s) }
  float        { \p s -> TokWSpan KW_Float        (posn p s) }
  for          { \p s -> TokWSpan KW_For          (posn p s) }
  goto         { \p s -> TokWSpan KW_Goto         (posn p s) }
  if           { \p s -> TokWSpan KW_If           (posn p s) }
  implements   { \p s -> TokWSpan KW_Implements   (posn p s) }
  import       { \p s -> TokWSpan KW_Import       (posn p s) }
  instanceof   { \p s -> TokWSpan KW_Instanceof   (posn p s) }
  int          { \p s -> TokWSpan KW_Int          (posn p s) }
  interface    { \p s -> TokWSpan KW_Interface    (posn p s) }
  long         { \p s -> TokWSpan KW_Long         (posn p s) }
  native       { \p s -> TokWSpan KW_Native       (posn p s) }
  new          { \p s -> TokWSpan KW_New          (posn p s) }
  package      { \p s -> TokWSpan KW_Package      (posn p s) }
  private      { \p s -> TokWSpan KW_Private      (posn p s) }
  protected    { \p s -> TokWSpan KW_Protected    (posn p s) }
  public       { \p s -> TokWSpan KW_Public       (posn p s) }
  return       { \p s -> TokWSpan KW_Return       (posn p s) }
  short        { \p s -> TokWSpan KW_Short        (posn p s) }
  static       { \p s -> TokWSpan KW_Static       (posn p s) }
  strictfp     { \p s -> TokWSpan KW_Strictfp     (posn p s) }
  super        { \p s -> TokWSpan KW_Super        (posn p s) }
  switch       { \p s -> TokWSpan KW_Switch       (posn p s) }
  synchronized { \p s -> TokWSpan KW_Synchronized (posn p s) }
  this         { \p s -> TokWSpan KW_This         (posn p s) }
  throw        { \p s -> TokWSpan KW_Throw        (posn p s) }
  throws       { \p s -> TokWSpan KW_Throws       (posn p s) }
  transient    { \p s -> TokWSpan KW_Transient    (posn p s) }
  try          { \p s -> TokWSpan KW_Try          (posn p s) }
  void         { \p s -> TokWSpan KW_Void         (posn p s) }
  volatile     { \p s -> TokWSpan KW_Volatile     (posn p s) }
  while        { \p s -> TokWSpan KW_While        (posn p s) }
  -- Paragon specific
  actor        { \p s -> TokWSpan KW_P_Actor      (posn p s) }
  close        { \p s -> TokWSpan KW_P_Close      (posn p s) }
  lock         { \p s -> TokWSpan KW_P_Lock       (posn p s) }
  notnull      { \p s -> TokWSpan KW_P_Notnull    (posn p s) }
  open         { \p s -> TokWSpan KW_P_Open       (posn p s) }
  policy       { \p s -> TokWSpan KW_P_Policy     (posn p s) }
  when         { \p s -> TokWSpan KW_P_When       (posn p s) }
  typemethod   { \p s -> TokWSpan KW_P_Typemethod (posn p s) }
  policyof     { \p s -> TokWSpan KW_P_Policyof   (posn p s) }
  readonly     { \p s -> TokWSpan KW_P_Readonly   (posn p s) }
  reflexive    { \p s -> TokWSpan KW_P_Reflexive  (posn p s) }
  transitive   { \p s -> TokWSpan KW_P_Transitive (posn p s) }
  symmetric    { \p s -> TokWSpan KW_P_Symmetric  (posn p s) }

  -- Separators
  "(" { \p s -> TokWSpan OpenParen   (posn p s) }
  ")" { \p s -> TokWSpan CloseParen  (posn p s) }
  "[" { \p s -> TokWSpan OpenSquare  (posn p s) }
  "]" { \p s -> TokWSpan CloseSquare (posn p s) }
  "{" { \p s -> TokWSpan OpenCurly   (posn p s) }
  "}" { \p s -> TokWSpan CloseCurly  (posn p s) }
  ";" { \p s -> TokWSpan SemiColon   (posn p s) }
  "," { \p s -> TokWSpan Comma       (posn p s) }
  "." { \p s -> TokWSpan Period      (posn p s) }

  -- Literals
  true  { \p s -> TokWSpan (BoolLit True)  (posn p s) }
  false { \p s -> TokWSpan (BoolLit False) (posn p s) }
  null  { \p s -> TokWSpan NullLit         (posn p s) }

  -- Identifiers
  $javaLetter $javaLetterOrDigit* { \p s -> TokWSpan (IdTok s) (posn p s) }

  -- Operators
  "="    { \p s -> TokWSpan Op_Assign             (posn p s) }
  ">"    { \p s -> TokWSpan Op_GT                 (posn p s) }
  "<"    { \p s -> TokWSpan Op_LT                 (posn p s) }
  "!"    { \p s -> TokWSpan Op_Bang               (posn p s) }
  "~"    { \p s -> TokWSpan Op_Tilde              (posn p s) }
  "=="   { \p s -> TokWSpan Op_Equal              (posn p s) }
  "<="   { \p s -> TokWSpan Op_LE                 (posn p s) }
  ">="   { \p s -> TokWSpan Op_GE                 (posn p s) }
  "!="   { \p s -> TokWSpan Op_NotEq              (posn p s) }
  "&&"   { \p s -> TokWSpan Op_And                (posn p s) }
  "||"   { \p s -> TokWSpan Op_Or                 (posn p s) }
  "++"   { \p s -> TokWSpan Op_Inc                (posn p s) }
  "--"   { \p s -> TokWSpan Op_Dec                (posn p s) }
  "+"    { \p s -> TokWSpan Op_Plus               (posn p s) }
  "-"    { \p s -> TokWSpan Op_Minus              (posn p s) }
  "*"    { \p s -> TokWSpan Op_Star               (posn p s) }
  "/"    { \p s -> TokWSpan Op_Slash              (posn p s) }
  "&"    { \p s -> TokWSpan Op_BitAnd             (posn p s) }
  "|"    { \p s -> TokWSpan Op_BitOr              (posn p s) }
  "^"    { \p s -> TokWSpan Op_Caret              (posn p s) }
  "%"    { \p s -> TokWSpan Op_Percent            (posn p s) }
  "<<"   { \p s -> TokWSpan Op_LShift             (posn p s) }
  ">>"   { \p s -> TokWSpan Op_RShift             (posn p s) }
  ">>>"  { \p s -> TokWSpan Op_UnSignRShift       (posn p s) }
  "+="   { \p s -> TokWSpan Op_PlusAssign         (posn p s) }
  "-="   { \p s -> TokWSpan Op_MinusAssign        (posn p s) }
  "*="   { \p s -> TokWSpan Op_StarAssign         (posn p s) }
  "/="   { \p s -> TokWSpan Op_SlashAssign        (posn p s) }
  "&="   { \p s -> TokWSpan Op_BitAndAssign       (posn p s) }
  "|="   { \p s -> TokWSpan Op_BitOrAssign        (posn p s) }
  "^="   { \p s -> TokWSpan Op_CaretAssign        (posn p s) }
  "%="   { \p s -> TokWSpan Op_PercentAssign      (posn p s) }
  "<<="  { \p s -> TokWSpan Op_LShiftAssign       (posn p s) }
  ">>="  { \p s -> TokWSpan Op_RShiftAssign       (posn p s) }
  ">>>=" { \p s -> TokWSpan Op_UnSignRShiftAssign (posn p s) }

  -- Symbols
  "?"   { \p s -> TokWSpan Question (posn p s) }
  ":"   { \p s -> TokWSpan Colon    (posn p s) }
  "@"   { \p s -> TokWSpan AtSign   (posn p s) }
  "..." { \p s -> TokWSpan Ellipsis (posn p s) }

{

-- | Data type representing tokens.
data Token
    -- Keywords
    = KW_Abstract
    | KW_Assert
    | KW_Boolean
    | KW_Break
    | KW_Byte
    | KW_Case
    | KW_Catch
    | KW_Char
    | KW_Class
    | KW_Const
    | KW_Continue
    | KW_Default
    | KW_Do
    | KW_Double
    | KW_Else
    | KW_Enum
    | KW_Extends
    | KW_Final
    | KW_Finally
    | KW_Float
    | KW_For
    | KW_Goto
    | KW_If
    | KW_Implements
    | KW_Import
    | KW_Instanceof
    | KW_Int
    | KW_Interface
    | KW_Long
    | KW_Native
    | KW_New
    | KW_Package
    | KW_Private
    | KW_Protected
    | KW_Public
    | KW_Return
    | KW_Short
    | KW_Static
    | KW_Strictfp
    | KW_Super
    | KW_Switch
    | KW_Synchronized
    | KW_This
    | KW_Throw
    | KW_Throws
    | KW_Transient
    | KW_Try
    | KW_Void
    | KW_Volatile
    | KW_While
    -- Paragon specific
    | KW_P_Actor
    | KW_P_Close
    | KW_P_Lock
    | KW_P_Notnull
    | KW_P_Open
    | KW_P_Policy
    | KW_P_When
    | KW_P_Typemethod
    | KW_P_Policyof
    | KW_P_Readonly
    | KW_P_Reflexive
    | KW_P_Transitive
    | KW_P_Symmetric

    -- Separators
    | OpenParen
    | CloseParen
    | OpenSquare
    | CloseSquare
    | OpenCurly
    | CloseCurly
    | SemiColon
    | Comma
    | Period

    -- Literals
    | IntLit Integer
    | LongLit Integer
    | DoubleLit Double
    | FloatLit Double
    | CharLit Char
    | StringLit String
    | BoolLit Bool
    | NullLit

    -- Identifiers
    | IdTok String

    -- Operators
    | Op_Assign
    | Op_GT
    | Op_LT
    | Op_Bang
    | Op_Tilde
    | Op_Equal
    | Op_LE
    | Op_GE
    | Op_NotEq
    | Op_And
    | Op_Or
    | Op_Inc
    | Op_Dec
    | Op_Plus
    | Op_Minus
    | Op_Star
    | Op_Slash
    | Op_BitAnd
    | Op_BitOr
    | Op_Caret
    | Op_Percent
    | Op_LShift
    | Op_RShift
    | Op_UnSignRShift
    | Op_PlusAssign
    | Op_MinusAssign
    | Op_StarAssign
    | Op_SlashAssign
    | Op_BitAndAssign
    | Op_BitOrAssign
    | Op_CaretAssign
    | Op_PercentAssign
    | Op_LShiftAssign
    | Op_RShiftAssign
    | Op_UnSignRShiftAssign

    -- Symbols
    | Question
    | Colon
    | AtSign
    | Ellipsis
  deriving Eq

-- | Token with it's source span.
data TokenWithSpan = TokWSpan
  { twsTok     :: Token
  , twsSrcSpan :: SrcSpan
  } deriving Eq

instance Show Token where
  -- Keywords
  show KW_Abstract     = "abstract"
  show KW_Assert       = "assert"
  show KW_Boolean      = "boolean"
  show KW_Break        = "break"
  show KW_Byte         = "byte"
  show KW_Case         = "case"
  show KW_Catch        = "catch"
  show KW_Char         = "char"
  show KW_Class        = "class"
  show KW_Const        = "const"
  show KW_Continue     = "continue"
  show KW_Default      = "default"
  show KW_Do           = "do"
  show KW_Double       = "double"
  show KW_Else         = "else"
  show KW_Enum         = "enum"
  show KW_Extends      = "extends"
  show KW_Final        = "final"
  show KW_Finally      = "finally"
  show KW_Float        = "float"
  show KW_For          = "for"
  show KW_Goto         = "goto"
  show KW_If           = "if"
  show KW_Implements   = "implements"
  show KW_Import       = "import"
  show KW_Instanceof   = "instanceof"
  show KW_Int          = "int"
  show KW_Interface    = "interface"
  show KW_Long         = "long"
  show KW_Native       = "native"
  show KW_New          = "new"
  show KW_Package      = "package"
  show KW_Private      = "private"
  show KW_Protected    = "protected"
  show KW_Public       = "public"
  show KW_Return       = "return"
  show KW_Short        = "short"
  show KW_Static       = "static"
  show KW_Strictfp     = "strictfp"
  show KW_Super        = "super"
  show KW_Switch       = "switch"
  show KW_Synchronized = "synchronized"
  show KW_This         = "this"
  show KW_Throw        = "throw"
  show KW_Throws       = "throws"
  show KW_Transient    = "transient"
  show KW_Try          = "try"
  show KW_Void         = "void"
  show KW_Volatile     = "volatile"
  show KW_While        = "while"
  -- Paragon specific
  show KW_P_Actor      = "actor"
  show KW_P_Close      = "close"
  show KW_P_Lock       = "lock"
  show KW_P_Notnull    = "notnull"
  show KW_P_Open       = "open"
  show KW_P_Policy     = "policy"
  show KW_P_When       = "when"
  show KW_P_Typemethod = "typemethod"
  show KW_P_Policyof   = "policyof"
  show KW_P_Readonly   = "readonly"
  show KW_P_Reflexive  = "reflexive"
  show KW_P_Transitive = "transitive"
  show KW_P_Symmetric  = "symmetric"
  -- Separators
  show OpenParen   = "("
  show CloseParen  = ")"
  show OpenSquare  = "["
  show CloseSquare = "]"
  show OpenCurly   = "{"
  show CloseCurly  = "}"
  show SemiColon   = ";"
  show Comma       = ","
  show Period      = "."
  -- Literals
  show (IntLit i)      = show i
  show (LongLit l)     = show l
  show (DoubleLit d)   = show d
  show (FloatLit f)    = show f
  show (CharLit c)     = show c
  show (StringLit str) = str
  show (BoolLit b)     = if b then "true" else "false"
  show NullLit         = "null"
  -- Identifiers
  show (IdTok ident) = ident
  -- Operators
  show Op_Assign             = "="
  show Op_GT                 = ">"
  show Op_LT                 = "<"
  show Op_Bang               = "!"
  show Op_Tilde              = "~"
  show Op_Equal              = "=="
  show Op_LE                 = "<="
  show Op_GE                 = ">="
  show Op_NotEq              = "!="
  show Op_And                = "&&"
  show Op_Or                 = "||"
  show Op_Inc                = "++"
  show Op_Dec                = "--"
  show Op_Plus               = "+"
  show Op_Minus              = "-"
  show Op_Star               = "*"
  show Op_Slash              = "/"
  show Op_BitAnd             = "&"
  show Op_BitOr              = "|"
  show Op_Caret              = "^"
  show Op_Percent            = "%"
  show Op_LShift             = "<<"
  show Op_RShift             = ">>"
  show Op_UnSignRShift       = ">>>"
  show Op_PlusAssign         = "+="
  show Op_MinusAssign        = "-="
  show Op_StarAssign         = "*="
  show Op_SlashAssign        = "/="
  show Op_BitAndAssign       = "&="
  show Op_BitOrAssign        = "|="
  show Op_CaretAssign        = "^="
  show Op_PercentAssign      = "%="
  show Op_LShiftAssign       = "<<="
  show Op_RShiftAssign       = ">>="
  show Op_UnSignRShiftAssign = ">>>="
  -- Symbols
  show Question = "?"
  show Colon    = ":"
  show AtSign   = "@"
  show Ellipsis = "..."

instance Show TokenWithSpan where
  show (TokWSpan t _) = show t

  showList ts s = intercalate ", " (map show ts) ++ s

-- | Converts Alex source position to Paragon source span.
-- Takes token string for length calculation.
-- NONAME is used as a source file name. Should be replaced in the parser.
posn :: AlexPosn -> String -> SrcSpan
posn (AlexPn _ l c) tokStr = SrcSpan "NONAME" l c l (c + length tokStr - 1)

-- | Top-level lexing function.
lexer :: String -> [TokenWithSpan]
lexer = alexScanTokens

}
