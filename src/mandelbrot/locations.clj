(ns mandelbrot.locations)

(defn cast-values-using [caster location-map]
  (into {}
    (for [[k v] location-map]
      [k (caster v)])))

(def save-map {:x-min -0.8208984375000001, :x-max -0.7857421875, :y-min -0.20039062500000004, :y-max -0.16523437500000004})

(def spiral-map {:x-min -0.8314329599999993, :x-max -0.8277004799999993, :y-min -0.23514623999999984, :y-max -0.23141375999999986})

(def full-map  {:x-min -1.8, :x-max 1.8, :y-min -1.8, :y-max 1.8})