module ASCII.Caseless
  ( -- * The @CaselessChar@ type
    CaselessChar (..),

    -- * Enumeration
    allCharacters,

    -- * Conversion
    disregardCase,
    toCase,
    assumeCaseUnsafe,
  )
where

import ASCII.Case (Case (..))
import ASCII.Char qualified as ASCII
import Data.Bool (otherwise)
import Data.Bool qualified as Bool
import Data.Char qualified as C
import Data.Data (Data)
import Data.Eq (Eq, (/=), (==))
import Data.Hashable (Hashable)
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord, (<), (<=), (>))
import GHC.Generics (Generic)
import Text.Show (Show)
import Prelude (Bounded, Enum, enumFromTo, fromEnum, maxBound, minBound, toEnum, (+), (-))
import Prelude qualified as Enum (Enum (..))

-- | A character in the ASCII character set, without an
-- upper/lower case distinction for letters
data CaselessChar =
      Null | StartOfHeading | StartOfText | EndOfText
    | EndOfTransmission | Enquiry | Acknowledgement | Bell
    | Backspace | HorizontalTab | LineFeed | VerticalTab
    | FormFeed | CarriageReturn | ShiftOut | ShiftIn | DataLinkEscape

    | DeviceControl1 | DeviceControl2 | DeviceControl3 | DeviceControl4

    | NegativeAcknowledgement | SynchronousIdle | EndOfTransmissionBlock
    | Cancel | EndOfMedium | Substitute | Escape

    | FileSeparator | GroupSeparator | RecordSeparator | UnitSeparator

    | Space | ExclamationMark | QuotationMark | NumberSign
    | DollarSign | PercentSign | Ampersand | Apostrophe
    | LeftParenthesis | RightParenthesis | Asterisk
    | PlusSign | Comma | HyphenMinus | FullStop | Slash

    | Digit0 | Digit1 | Digit2 | Digit3 | Digit4
    | Digit5 | Digit6 | Digit7 | Digit8 | Digit9

    | Colon | Semicolon | LessThanSign | EqualsSign
    | GreaterThanSign | QuestionMark | AtSign

    | LetterA | LetterB | LetterC | LetterD | LetterE | LetterF
    | LetterG | LetterH | LetterI | LetterJ | LetterK | LetterL
    | LetterM | LetterN | LetterO | LetterP | LetterQ | LetterR
    | LetterS | LetterT | LetterU | LetterV | LetterW | LetterX
    | LetterY | LetterZ

    | LeftSquareBracket | Backslash | RightSquareBracket
    | Caret | Underscore | GraveAccent

    | LeftCurlyBracket | VerticalLine | RightCurlyBracket
    | Tilde | Delete

-- | ASCII characters can be compared for equality using '(==)'.
deriving stock instance Eq CaselessChar

-- | Caseless ASCII characters are ordered; for example, the letter
-- /A/ is "less than" ('<') the letter /B/ because it appears earlier
-- in the list. The ordering of caseless ASCII characters is roughly
-- the same as the ordering of the corresponding Unicode 'C.Char's,
-- with caseless letters appearing in the place of case-sensitive
-- capital letters.
deriving stock instance Ord CaselessChar

-- | The 'Enum' instance allows us to use range syntax, for example
-- @['LetterA' .. 'LetterZ']@ is a list all letters from /a/ to /z/.
deriving stock instance Enum CaselessChar

-- | You can write @(['minBound' .. 'maxBound'] :: ['CaselessChar'])@
-- to get a list of all the caseless ASCII characters.
deriving stock instance Bounded CaselessChar

-- | 'show' produces the name of a constructor. For example,
-- the character @e@ is shown as “@LetterE@”.
-- See "ASCII.Caseless" for the complete list of constructor names.
deriving stock instance Show CaselessChar

deriving stock instance Data CaselessChar

deriving stock instance Generic CaselessChar

deriving anyclass instance Hashable CaselessChar

-- | There are 102 characters in total.
allCharacters :: [CaselessChar]
allCharacters = Prelude.enumFromTo Prelude.minBound Prelude.maxBound

disregardCase :: ASCII.Char -> CaselessChar
disregardCase x
  | x <= ASCII.GraveAccent = Enum.toEnum (Enum.fromEnum x)
  | x <= ASCII.SmallLetterZ = Enum.toEnum (Enum.fromEnum x - 32)
  | Bool.otherwise = Enum.toEnum (Enum.fromEnum x - 26)

-- | Like 'disregardCase', but defined only where the character
-- is either a letter in the given case or a non-letter
--
-- For upper case, this is slightly more efficient than 'disregardCase'.
assumeCaseUnsafe :: Case -> ASCII.Char -> CaselessChar
assumeCaseUnsafe LowerCase x = disregardCase x
assumeCaseUnsafe UpperCase x
  | x <= ASCII.GraveAccent = Enum.toEnum (Enum.fromEnum x)
  | Bool.otherwise = Enum.toEnum (Enum.fromEnum x - 26)

toCase :: Case -> CaselessChar -> ASCII.Char
toCase UpperCase x
  | x <= GraveAccent = Enum.toEnum (Enum.fromEnum x)
  | Bool.otherwise = Enum.toEnum (Enum.fromEnum x + 26)
toCase LowerCase x
  | x <= AtSign = Enum.toEnum (Enum.fromEnum x)
  | x <= LetterZ = Enum.toEnum (Enum.fromEnum x + 32)
  | x <= GraveAccent = Enum.toEnum (Enum.fromEnum x)
  | Bool.otherwise = Enum.toEnum (Enum.fromEnum x + 26)
