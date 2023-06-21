module Main (main) where

import ASCII.Case (Case (..))
import ASCII.Caseless
  ( CaselessChar,
    allCharacters,
    assumeCaseUnsafe,
    disregardCase,
    toCase,
  )
import ASCII.Caseless qualified as Caseless
import ASCII.Char qualified as CaseSensitive
import Data.Eq (Eq ((==)))
import Data.Foldable (for_)
import Data.Function (($))
import Data.List (length)
import Data.String (String)
import System.IO (IO)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec do
  describe "caseless characters" do
    it "how many" $ length allCharacters == 102

  describe "disregardCase" do
    for_ [nonLetters, capitalLetters, smallLetters] \(name, pairs) ->
      it name $ for_ pairs $ \(a, b) -> disregardCase b `shouldBe` a

  describe "testAssumeCaseUnsafe" do
    describe "UpperCase" do
      for_ [nonLetters, capitalLetters] \(name, pairs) ->
        it name $ for_ pairs $ \(a, b) -> assumeCaseUnsafe UpperCase b `shouldBe` a
    describe "LowerCase" do
      for_ [nonLetters, smallLetters] \(name, pairs) ->
        it name $ for_ pairs $ \(a, b) -> assumeCaseUnsafe LowerCase b `shouldBe` a

  describe "toCase" do
    describe "UpperCase" do
      for_ [nonLetters, capitalLetters] \(name, pairs) ->
        it name $ for_ pairs \(a, b) -> toCase UpperCase a `shouldBe` b
    describe "LowerCase" do
      for_ [nonLetters, smallLetters] \(name, pairs) ->
        it name $ for_ pairs \(a, b) -> toCase LowerCase a `shouldBe` b

nonLetters, smallLetters, capitalLetters :: (String, [(CaselessChar, CaseSensitive.Char)])
smallLetters =
  (,)
    "small letters"
    [ (,) Caseless.LetterA CaseSensitive.SmallLetterA,
      (,) Caseless.LetterB CaseSensitive.SmallLetterB,
      (,) Caseless.LetterC CaseSensitive.SmallLetterC,
      (,) Caseless.LetterD CaseSensitive.SmallLetterD,
      (,) Caseless.LetterE CaseSensitive.SmallLetterE,
      (,) Caseless.LetterF CaseSensitive.SmallLetterF,
      (,) Caseless.LetterG CaseSensitive.SmallLetterG,
      (,) Caseless.LetterH CaseSensitive.SmallLetterH,
      (,) Caseless.LetterI CaseSensitive.SmallLetterI,
      (,) Caseless.LetterJ CaseSensitive.SmallLetterJ,
      (,) Caseless.LetterK CaseSensitive.SmallLetterK,
      (,) Caseless.LetterL CaseSensitive.SmallLetterL,
      (,) Caseless.LetterM CaseSensitive.SmallLetterM,
      (,) Caseless.LetterN CaseSensitive.SmallLetterN,
      (,) Caseless.LetterO CaseSensitive.SmallLetterO,
      (,) Caseless.LetterP CaseSensitive.SmallLetterP,
      (,) Caseless.LetterQ CaseSensitive.SmallLetterQ,
      (,) Caseless.LetterR CaseSensitive.SmallLetterR,
      (,) Caseless.LetterS CaseSensitive.SmallLetterS,
      (,) Caseless.LetterT CaseSensitive.SmallLetterT,
      (,) Caseless.LetterU CaseSensitive.SmallLetterU,
      (,) Caseless.LetterV CaseSensitive.SmallLetterV,
      (,) Caseless.LetterW CaseSensitive.SmallLetterW,
      (,) Caseless.LetterX CaseSensitive.SmallLetterX,
      (,) Caseless.LetterY CaseSensitive.SmallLetterY,
      (,) Caseless.LetterZ CaseSensitive.SmallLetterZ
    ]
capitalLetters =
  (,)
    "capital letters"
    [ (,) Caseless.LetterA CaseSensitive.CapitalLetterA,
      (,) Caseless.LetterB CaseSensitive.CapitalLetterB,
      (,) Caseless.LetterC CaseSensitive.CapitalLetterC,
      (,) Caseless.LetterD CaseSensitive.CapitalLetterD,
      (,) Caseless.LetterE CaseSensitive.CapitalLetterE,
      (,) Caseless.LetterF CaseSensitive.CapitalLetterF,
      (,) Caseless.LetterG CaseSensitive.CapitalLetterG,
      (,) Caseless.LetterH CaseSensitive.CapitalLetterH,
      (,) Caseless.LetterI CaseSensitive.CapitalLetterI,
      (,) Caseless.LetterJ CaseSensitive.CapitalLetterJ,
      (,) Caseless.LetterK CaseSensitive.CapitalLetterK,
      (,) Caseless.LetterL CaseSensitive.CapitalLetterL,
      (,) Caseless.LetterM CaseSensitive.CapitalLetterM,
      (,) Caseless.LetterN CaseSensitive.CapitalLetterN,
      (,) Caseless.LetterO CaseSensitive.CapitalLetterO,
      (,) Caseless.LetterP CaseSensitive.CapitalLetterP,
      (,) Caseless.LetterQ CaseSensitive.CapitalLetterQ,
      (,) Caseless.LetterR CaseSensitive.CapitalLetterR,
      (,) Caseless.LetterS CaseSensitive.CapitalLetterS,
      (,) Caseless.LetterT CaseSensitive.CapitalLetterT,
      (,) Caseless.LetterU CaseSensitive.CapitalLetterU,
      (,) Caseless.LetterV CaseSensitive.CapitalLetterV,
      (,) Caseless.LetterW CaseSensitive.CapitalLetterW,
      (,) Caseless.LetterX CaseSensitive.CapitalLetterX,
      (,) Caseless.LetterY CaseSensitive.CapitalLetterY,
      (,) Caseless.LetterZ CaseSensitive.CapitalLetterZ
    ]
nonLetters =
  (,)
    "non-letters"
    [ (,) Caseless.Null CaseSensitive.Null,
      (,) Caseless.StartOfHeading CaseSensitive.StartOfHeading,
      (,) Caseless.StartOfText CaseSensitive.StartOfText,
      (,) Caseless.EndOfText CaseSensitive.EndOfText,
      (,) Caseless.EndOfTransmission CaseSensitive.EndOfTransmission,
      (,) Caseless.Enquiry CaseSensitive.Enquiry,
      (,) Caseless.Acknowledgement CaseSensitive.Acknowledgement,
      (,) Caseless.Bell CaseSensitive.Bell,
      (,) Caseless.Backspace CaseSensitive.Backspace,
      (,) Caseless.HorizontalTab CaseSensitive.HorizontalTab,
      (,) Caseless.LineFeed CaseSensitive.LineFeed,
      (,) Caseless.VerticalTab CaseSensitive.VerticalTab,
      (,) Caseless.FormFeed CaseSensitive.FormFeed,
      (,) Caseless.CarriageReturn CaseSensitive.CarriageReturn,
      (,) Caseless.ShiftOut CaseSensitive.ShiftOut,
      (,) Caseless.ShiftIn CaseSensitive.ShiftIn,
      (,) Caseless.DataLinkEscape CaseSensitive.DataLinkEscape,
      (,) Caseless.DeviceControl1 CaseSensitive.DeviceControl1,
      (,) Caseless.DeviceControl2 CaseSensitive.DeviceControl2,
      (,) Caseless.DeviceControl3 CaseSensitive.DeviceControl3,
      (,) Caseless.DeviceControl4 CaseSensitive.DeviceControl4,
      (,) Caseless.NegativeAcknowledgement CaseSensitive.NegativeAcknowledgement,
      (,) Caseless.SynchronousIdle CaseSensitive.SynchronousIdle,
      (,) Caseless.EndOfTransmissionBlock CaseSensitive.EndOfTransmissionBlock,
      (,) Caseless.Cancel CaseSensitive.Cancel,
      (,) Caseless.EndOfMedium CaseSensitive.EndOfMedium,
      (,) Caseless.Substitute CaseSensitive.Substitute,
      (,) Caseless.Escape CaseSensitive.Escape,
      (,) Caseless.FileSeparator CaseSensitive.FileSeparator,
      (,) Caseless.GroupSeparator CaseSensitive.GroupSeparator,
      (,) Caseless.RecordSeparator CaseSensitive.RecordSeparator,
      (,) Caseless.UnitSeparator CaseSensitive.UnitSeparator,
      (,) Caseless.Space CaseSensitive.Space,
      (,) Caseless.ExclamationMark CaseSensitive.ExclamationMark,
      (,) Caseless.QuotationMark CaseSensitive.QuotationMark,
      (,) Caseless.NumberSign CaseSensitive.NumberSign,
      (,) Caseless.DollarSign CaseSensitive.DollarSign,
      (,) Caseless.PercentSign CaseSensitive.PercentSign,
      (,) Caseless.Ampersand CaseSensitive.Ampersand,
      (,) Caseless.Apostrophe CaseSensitive.Apostrophe,
      (,) Caseless.LeftParenthesis CaseSensitive.LeftParenthesis,
      (,) Caseless.RightParenthesis CaseSensitive.RightParenthesis,
      (,) Caseless.Asterisk CaseSensitive.Asterisk,
      (,) Caseless.PlusSign CaseSensitive.PlusSign,
      (,) Caseless.Comma CaseSensitive.Comma,
      (,) Caseless.HyphenMinus CaseSensitive.HyphenMinus,
      (,) Caseless.FullStop CaseSensitive.FullStop,
      (,) Caseless.Slash CaseSensitive.Slash,
      (,) Caseless.Digit0 CaseSensitive.Digit0,
      (,) Caseless.Digit1 CaseSensitive.Digit1,
      (,) Caseless.Digit2 CaseSensitive.Digit2,
      (,) Caseless.Digit3 CaseSensitive.Digit3,
      (,) Caseless.Digit4 CaseSensitive.Digit4,
      (,) Caseless.Digit5 CaseSensitive.Digit5,
      (,) Caseless.Digit6 CaseSensitive.Digit6,
      (,) Caseless.Digit7 CaseSensitive.Digit7,
      (,) Caseless.Digit8 CaseSensitive.Digit8,
      (,) Caseless.Digit9 CaseSensitive.Digit9,
      (,) Caseless.Colon CaseSensitive.Colon,
      (,) Caseless.Semicolon CaseSensitive.Semicolon,
      (,) Caseless.LessThanSign CaseSensitive.LessThanSign,
      (,) Caseless.EqualsSign CaseSensitive.EqualsSign,
      (,) Caseless.GreaterThanSign CaseSensitive.GreaterThanSign,
      (,) Caseless.QuestionMark CaseSensitive.QuestionMark,
      (,) Caseless.AtSign CaseSensitive.AtSign,
      (,) Caseless.LeftSquareBracket CaseSensitive.LeftSquareBracket,
      (,) Caseless.Backslash CaseSensitive.Backslash,
      (,) Caseless.RightSquareBracket CaseSensitive.RightSquareBracket,
      (,) Caseless.Caret CaseSensitive.Caret,
      (,) Caseless.Underscore CaseSensitive.Underscore,
      (,) Caseless.GraveAccent CaseSensitive.GraveAccent,
      (,) Caseless.LeftCurlyBracket CaseSensitive.LeftCurlyBracket,
      (,) Caseless.VerticalLine CaseSensitive.VerticalLine,
      (,) Caseless.RightCurlyBracket CaseSensitive.RightCurlyBracket,
      (,) Caseless.Tilde CaseSensitive.Tilde,
      (,) Caseless.Delete CaseSensitive.Delete
    ]
