(ns mandelbrot.locations)

(defn cast-values-using [caster location-map]
  (into {}
    (for [[k v] location-map]
      [k (caster v)])))

(def save-map {:x-min -0.8208984375000001, :x-max -0.7857421875, :y-min -0.20039062500000004, :y-max -0.16523437500000004})

(def spiral-map {:x-min -0.8314329599999993, :x-max -0.8277004799999993, :y-min -0.23514623999999984, :y-max -0.23141375999999986})

(def full-map  {:x-min -1.8, :x-max 1.8, :y-min -1.8, :y-max 1.8})

(def save-map2 {:x-min -0.16445159999999989496608301942082164259772980585694M, :x-max -0.16441559999999988140586756890648700846213614568114M, :y-min -1.0403004705882352736085773303731372152469702996314M, :y-max -1.0402644705882351212704838017142350281574181281030M})
