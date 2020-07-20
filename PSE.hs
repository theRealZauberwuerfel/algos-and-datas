data PSElement =
    H                                                                                  | He
  | Li | Be                                                   | B  | C  | N  | O  | F  | Ne
  | Na | Mg                                                   | Al | Si | P  | S  | Cl | Ar
  | K  | Ca | Sc | Ti | V  | Cr | Mn | Fe | Co | Ni | Cu | Zn | Ga | Ge | As | Se | Br | Kr
  | Rb | Sr | Y  | Zr | Nb | Mo | Tc | Ru | Rh | Pd | Ag | Cd | In | Sn | Sb | Te | I  | Xe
  deriving (Enum,Ord,Show,Read)

data ElementClass =
    AlkaliMetal
  | EarthAlkaliMetal
  | HalfMetal
  | OtherMetal
  | TransitionMetal
  | NotMetal
  | Halogene

data AggregateState = BoseEinstein | Solid | Fluid | Gas | Plasma

data Element = Element {
    abbreviation :: PSElement
  , englishName :: String
  , latinName :: String
  , group :: Int
  , period :: Int
  , classification :: ElementClass
  , relativeAtomMass :: Double
  , allredRochowElectroNegativity :: Float
  , paulingElectroNegativity :: Float
  , atomRadius :: Float
  , electronConfiguration }
