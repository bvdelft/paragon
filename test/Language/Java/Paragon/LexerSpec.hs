-- | Lexer tests.
module Language.Java.Paragon.LexerSpec (main, spec) where

import Test.Hspec
import Language.Java.Paragon.Lexer

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- | Main specification function.
spec :: Spec
spec = do
  describe "lexer" $ do
    -- Success
    it "handles line comments correctly" $
      map twsTok
        (lexer
          "// comment\n \
         \ abstract")
      `shouldBe` [KW_Abstract]

    it "handles multi-line comments correctly" $
      map twsTok
        (lexer
          "/****\n \
          \ * Common multi-line comment style.\n \
          \ ****/\n \
          \ abstract\n \
          \/****\n \
          \ * Another common multi-line comment style.\n \
          \ */")
      `shouldBe` [KW_Abstract]

    it "recognizes all Java keywords" $
      map twsTok
        (lexer "abstract   continue   for          new         switch       \
              \ assert     default    if           package     synchronized \
              \ boolean    do         goto         private     this         \
              \ break      double     implements   protected   throw        \
              \ byte       else       import       public      throws       \
              \ case       enum       instanceof   return      transient    \
              \ catch      extends    int          short       try          \
              \ char       final      interface    static      void         \
              \ class      finally    long         strictfp    volatile     \
              \ const      float      native       super       while")
      `shouldBe` [ KW_Abstract , KW_Continue , KW_For        , KW_New       , KW_Switch
                 , KW_Assert   , KW_Default  , KW_If         , KW_Package   , KW_Synchronized
                 , KW_Boolean  , KW_Do       , KW_Goto       , KW_Private   , KW_This
                 , KW_Break    , KW_Double   , KW_Implements , KW_Protected , KW_Throw
                 , KW_Byte     , KW_Else     , KW_Import     , KW_Public    , KW_Throws
                 , KW_Case     , KW_Enum     , KW_Instanceof , KW_Return    , KW_Transient
                 , KW_Catch    , KW_Extends  , KW_Int        , KW_Short     , KW_Try
                 , KW_Char     , KW_Final    , KW_Interface  , KW_Static    , KW_Void
                 , KW_Class    , KW_Finally  , KW_Long       , KW_Strictfp  , KW_Volatile
                 , KW_Const    , KW_Float    , KW_Native     , KW_Super     , KW_While
                 ]

    it "recognizes all Paragon keywords" $
      map twsTok
        (lexer "actor close      lock     notnull  open      policy \
              \ when  typemethod policyof readonly reflexive transitive symmetric")
      `shouldBe` [ KW_P_Actor , KW_P_Close      , KW_P_Lock     , KW_P_Notnull  , KW_P_Open      , KW_P_Policy
                 , KW_P_When  , KW_P_Typemethod , KW_P_Policyof , KW_P_Readonly , KW_P_Reflexive , KW_P_Transitive , KW_P_Symmetric
                 ]

    it "recognizes all separators" $
      map twsTok (lexer "(    )    {    }    [    ]    ;    ,    .")
      `shouldBe`
      [ OpenParen, CloseParen, OpenCurly, CloseCurly, OpenSquare, CloseSquare, SemiColon, Comma, Period ]

    it "recognizes boolean literals and null" $
      map twsTok (lexer "true false null") `shouldBe` [ BoolLit True, BoolLit False, NullLit ]

    it "recognizes integer 0 literals" $
      map twsTok (lexer "0 0l 0L") `shouldBe` [ IntLit 0, LongLit 0, LongLit 0 ]

    it "recognizes integer decimal literals" $
      map twsTok (lexer "10 10l 10L") `shouldBe` [ IntLit 10, LongLit 10, LongLit 10 ]

    it "recognizes integer decimal literals with underscores" $
      map twsTok (lexer "1_0 1_0l 1_0L") `shouldBe` [ IntLit 10, LongLit 10, LongLit 10 ]

    it "recognizes integer hex literals" $
      map twsTok (lexer "0xa 0xA 0Xa 0XA 0xal 0xAl 0Xal 0XAl 0xaL 0xAL 0XaL 0XAL") `shouldBe` [ IntLit 10,  IntLit 10,  IntLit 10,  IntLit 10
                                                                                              , LongLit 10, LongLit 10, LongLit 10, LongLit 10
                                                                                              , LongLit 10, LongLit 10, LongLit 10, LongLit 10 ]

    it "recognizes integer hex literals with underscores" $
      map twsTok (lexer "0x1_f 0x1_F 0X1_f 0X1_F 0x1_fl 0x1_Fl 0X1_fl 0X1_Fl 0x1_fL 0x1_FL 0X1_fL 0X1_FL")
      `shouldBe`
      [ IntLit 31,  IntLit 31,  IntLit 31,  IntLit 31
      , LongLit 31, LongLit 31, LongLit 31, LongLit 31
      , LongLit 31, LongLit 31, LongLit 31, LongLit 31 ]

    it "recognizes integer octal literals" $
      map twsTok (lexer "01 01l 01L") `shouldBe` [ IntLit 1, LongLit 1, LongLit 1 ]

    it "recognizes integer octal literals with underscores" $
      map twsTok (lexer "0_1 0_1l 0_1L") `shouldBe` [ IntLit 1, LongLit 1, LongLit 1 ]

    it "recognizes integer binary literals" $
      map twsTok (lexer "0b10 0B10 0b10l 0B10l 0b10L 0B10L") `shouldBe` [ IntLit 2, IntLit 2, LongLit 2, LongLit 2, LongLit 2, LongLit 2 ]

    it "recognizes integer binary literals with underscores" $
      map twsTok (lexer "0b1_0 0B1_0 0b1_0l 0B1_0l 0b1_0L 0B1_0L") `shouldBe` [ IntLit 2, IntLit 2, LongLit 2, LongLit 2, LongLit 2, LongLit 2 ]

    it "recognizes identifiers" $
      map twsTok (lexer "x _x $x Var x123") `shouldBe` [ IdTok "x", IdTok "_x", IdTok "$x", IdTok "Var", IdTok "x123" ]

    it "recognizes all operators" $
      map twsTok
        (lexer "=   >   <   !   ~ \
              \ ==  <=  >=  !=    \
              \ &&  ||  ++  --    \
              \ +   -   *   /     \
              \ &   |   ^   %     \
              \ <<   >>   >>>     \
              \ +=  -=  *=  /=    \
              \ &=  |=  ^=  %=    \
              \ <<=  >>=  >>>=")
      `shouldBe` [ Op_Assign       , Op_GT           , Op_LT                 , Op_Bang          , Op_Tilde
                 , Op_Equal        , Op_LE           , Op_GE                 , Op_NotEq
                 , Op_And          , Op_Or           , Op_Inc                , Op_Dec
                 , Op_Plus         , Op_Minus        , Op_Star               , Op_Slash
                 , Op_BitAnd       , Op_BitOr        , Op_Caret              , Op_Percent
                 , Op_LShift       , Op_RShift       , Op_UnSignRShift
                 , Op_PlusAssign   , Op_MinusAssign  , Op_StarAssign         , Op_SlashAssign
                 , Op_BitAndAssign , Op_BitOrAssign  , Op_CaretAssign        , Op_PercentAssign
                 , Op_LShiftAssign , Op_RShiftAssign , Op_UnSignRShiftAssign
                 ]

    it "recognizes all symbols" $
      map twsTok (lexer "?  :  @  ...") `shouldBe` [ Question, Colon, AtSign, Ellipsis ]

