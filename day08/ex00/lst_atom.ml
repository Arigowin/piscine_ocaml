class hydrogen =
  object
    inherit Atom.atom as super
    method name = "Hydrogen"
    method symbol = "H"
    method atomic_number = 1
  end

class carbon =
  object
    inherit Atom.atom as super
    method name = "Carbon"
    method symbol = "C"
    method atomic_number = 6
  end

class nitrogen =
  object
    inherit Atom.atom as super
    method name = "Nitrogen"
    method symbol = "N"
    method atomic_number = 7
  end

class oxygen =
  object
    inherit Atom.atom as super
    method name = "Oxygen"
    method symbol = "O"
    method atomic_number = 8
  end

class zinc =
  object
    inherit Atom.atom as super
    method name = "Zinc"
    method symbol = "Zn"
    method atomic_number = 30
  end

class arsenic =
  object
    inherit Atom.atom as super
    method name = "Arsenic"
    method symbol = "As"
    method atomic_number = 33
  end

class platinum =
  object
    inherit Atom.atom as super
    method name = "Platinum"
    method symbol = "Pt"
    method atomic_number = 78
  end
