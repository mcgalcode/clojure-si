(ns citrine-challenge.si)

(def pi
  (. Math PI))

(def units
  {
    "minute" {
      :name "s"
      :multiplier 60.0
    }
    "min" {
      :name "s"
      :multiplier 60.0
    }
    "hour" {
      :name "s"
      :multiplier 3600.0
    }
    "h" {
      :name "s"
      :multiplier 3600.0
    }
    "day" {
      :name "s"
      :multiplier 86400.0
    }
    "d" {
      :name "s"
      :multiplier 86400.0
    }
    "degree" {
      :name "rad"
      :multiplier (/ pi 180.0)
    }
    "'" {
      :name "rad"
      :multiplier (/ pi 180.0)
    }
    "second" {
      :name "rad"
      :multiplier (/ pi 648000.0)
    }
    "\"" {
      :name "rad"
      :multiplier (/ pi 648000.0)
    }
    "hectare" {
      :name "m2"
      :multiplier 10000.0
    }
    "ha" {
      :name "m2"
      :multiplier 10000.0
    }
    "litre" {
      :name "m3"
      :multiplier 0.001
    }
    "L" {
      :name "m3"
      :multiplier 0.001
    }
    "tonne" {
      :name "kg"
      :multiplier 1000.0
    }
    "t" {
      :name "kg"
      :multiplier 1000.0
    }
  })