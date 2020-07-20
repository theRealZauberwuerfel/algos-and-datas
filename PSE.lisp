(defparameter *periodic-table*
  '( H                                                  He
     Li Be                               B  C  N  O  F  Ne
     Na Mg                               Al Si P  S  Cl Ar
     K  Ca Sc Ti V  Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr
     Rb Sr Y  Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I  Xe )

(deftype element-abbreviation ()
  `(member ,@*periodic-table*))

(deftype ELEMENT-CLASS ()
  '(member
    ALKALI-METAL
    EARTH-ALKALI-METAL
    HALF-METAL
    OTHER-METAL
    TRANSITION-METAL
    NOT-METAL
    HALOGENE
    ))

(deftype aggregate-state ()
  '(member BOSE-EINSTEIN SOLID FLUID GAS PLASMA))

(defstruct element
  (abbreviation nil :type element-abbreviation)
  (englishName nil :type (or string symbol))
  (latinName nil :type (or string symbol))
  (group 1 :type (integer 1))
  (period 1 :type (integer 1))
  (classification nil :type element-class)
  (relative-atom-mass 0.0 :type double)
  (allred-rochow-electro-negativity 0.0 :type (float 0.0 5.0))
  (pauling-electro-negativity 0.0 :type (float 0.0 5.0))
  (atom-radius 0.0 (float 0.0))
  electron-configuration)
