(ns mandelbrot.locations)

; FIXME: "rep" dimensions don't belong! Move to new record?

(def full-map
  {:start-r -1, :end-r 1,
   :start-i -1, :end-i 1,
   :rep-width nil, :rep-height nil})

(def center-spiral
  {:start-r 0.42078134894406044, :end-r 0.42819712586666975,
   :start-i 0.20381530471020423, :end-i 0.21123108163281337,
   :rep-width 600, :rep-height 600})

(def hand-of-god
  {:start-r 0.42218235623709, :end-r 0.42220408216487204, :start-i 0.20450319522662225, :end-i 0.20452492115440446, :rep-width 600, :rep-height 600})

(def super-zoom
  {:start-r 0.42219198184113976, :end-r 0.422192066708113, :start-i 0.20451335436068932, :end-i 0.20451343922766282, :rep-width 600, :rep-height 600})

(def galaxies
  {:start-r 0.37200794012911836, :end-r 0.37299782685583377, :start-i 0.15390684313272682, :end-i 0.1548967298594421, :rep-width 600, :rep-height 400.0})

(def swirl
  {:start-r 0.37245714285714276, :end-r 0.3744571428571429,
   :start-i -0.16159999999999997, :end-i -0.15959999999999996,
   :rep-width 700, :rep-height 700})

(def tentacle-example
  {:start-r -0.07237142857142852, :end-r -0.07037142857142852,
   :start-i -0.8299428571428572, :end-i -0.8279428571428572,
   :rep-width 700, :rep-height 466.6666666666667})

(def evolving-swirls
  {:start-r 0.32984386266094434, :end-r 0.3435618626609446,
   :start-i 0.3943739612756264, :end-i 0.4080919612756264})

(def tool-swirls
  {:start-r 0.3439489359715373, :end-r 0.3439489361715373,
   :start-i -0.0560718297288660, :end-i -0.0560718295288660})

(def crazy-swirl
  {:start-r 0.2773242138364778 :end-r 0.2803242138364779
   :start-i 0.0067561307901907 :end-i 0.0097561307901907})