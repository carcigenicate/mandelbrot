(ns mandelbrot.seesaw.save-manager
  (:require [mandelbrot.seesaw.id-generator :as idg]
            [mandelbrot.concurrent-finder :as cf]
            [mandelbrot.seesaw.multi-progress-bar :as mpb]

            [helpers.general-helpers :as g]

            [seesaw.core :as sc])

  (:import [java.awt.image BufferedImage]
           [java.awt Color]
           [java.io File]
           [javax.imageio ImageIO]))

(def save-path "./saves/")

(def color-mode BufferedImage/TYPE_INT_RGB)
(def save-ext "png")

(def update-perc 0.005)

(defn format-to-n-places [n n-places]
  (format (str "%." n-places "f") (double n)))

(defn new-save-manager
  ([save-root on-all-finish-f]
   {:save-root save-root, :id-generator (idg/new-id-generator 0),
    :on-all-finish-f on-all-finish-f, :last-ms-elapsed-atom (atom {})})

  ([save-root]
   (new-save-manager save-root (constantly nil))))

(defn new-image-for-limits ^BufferedImage [limits]
  (let [{:keys [rep-width rep-height]} limits
        ^BufferedImage img (BufferedImage. rep-width rep-height color-mode)]

    img))

(defn draw-image-in-limits [limits points color-f point-f on-finish-f]
  (let [{:keys [rep-width rep-height]} limits
        img (new-image-for-limits limits)]

    (loop [point-n 0
           [{:keys [r i rep-x rep-y iters] :as point} & rest-points] points]

      (point-f point-n point)

      (if point
        (let [^Color color (color-f r i iters)
              rgb (.getRGB color)]

          (.setRGB img rep-x rep-y rgb)

          (recur (inc point-n) rest-points))

        (do
          (on-finish-f)

          img)))))

(defn limits->std-name [limits]
  (let [{:keys [start-r end-r, start-i end-i]} limits
        f #(format-to-n-places % 16)]

    (str "["(f start-r) " " (f end-r) " " (f start-i) " " (f end-i) "]")))

(defn save-image [limits, color-opt-str, ^BufferedImage img]
  (let [file-name (str (limits->std-name limits)
                       "Date"  (g/current-ms-timestamp)
                       color-opt-str)
        path (str save-path file-name "." save-ext)]
    (clojure.java.io/make-parents path)

    (ImageIO/write img, ^String save-ext, (File. path))))

(defn minutes-remaining [jobs-completed total-jobs ms-elapsed]
  (if (zero? jobs-completed) ; TODO: Handle externally?
    0.0

    (let [ms-per (/ ms-elapsed jobs-completed)
          remaining-jobs (- total-jobs jobs-completed)
          ms-remaining (* remaining-jobs ms-per)]

      (double (/ ms-remaining 1000 60)))))

(defn- find-soonest-finishing [progress-snapshot last-elapsed]
  (when (seq progress-snapshot)
    (->> progress-snapshot
         (map (fn [[id {:keys [current-progress max-progress]}]]
                (minutes-remaining current-progress max-progress (last-elapsed id))))
         (apply min))))

(defn update-UI [time-label prog-bar last-elapsed-atom]
  (let [progress-snapshot (mpb/get-progress-snapshot prog-bar)
        last-elapsed-snapshot @last-elapsed-atom]

    (when-let [mins-left (find-soonest-finishing progress-snapshot last-elapsed-snapshot)]
      (sc/invoke-later
        (sc/text! time-label (str (format-to-n-places mins-left 2) " mins")))

      (sc/repaint! [time-label prog-bar]))))

(defn- register-save! [manager id total-pixels]
  (let [{:keys [save-root last-ms-elapsed-atom]} manager
        prog-bar (sc/select save-root [:#save-progress-bar])]

    (mpb/add-job! prog-bar id total-pixels)

    (swap! last-ms-elapsed-atom
      assoc id Long/MAX_VALUE)))

(defn- unregister-save! [manager id]
  (let [{:keys [save-root last-ms-elapsed-atom on-all-finish-f]} manager
        prog-bar (sc/select save-root [:#save-progress-bar])]

    (mpb/remove-job! prog-bar id)

    (swap! last-ms-elapsed-atom
      dissoc id)

    (when-not (seq (mpb/get-progress-snapshot prog-bar))
      (on-all-finish-f))))

(defn start-save [manager save-limits color-f]
  (let [{:keys [save-root on-all-finish-f id-generator last-ms-elapsed-atom]} manager
        {:keys [rep-width rep-height]} save-limits
        total (* rep-width rep-height)

        save-id (idg/generate-id! id-generator)]

    (register-save! manager save-id total)

    (let [color-opt-str (:color-opts (meta color-f))
          update-every (inc (int (* total update-perc)))

          points (cf/lazy-par-calc-points save-limits)

          cur-ms g/current-ms-timestamp
          start-ms (cur-ms)

          time-label (sc/select save-root [:#time-remaining])
          prog-bar (sc/select save-root [:#save-progress-bar]) ; TODO: Make sure to change in third-try!

          img (draw-image-in-limits save-limits points color-f
                (fn [n c]
                  (when (zero? (rem n update-every))
                    (let [elapsed (- (cur-ms) start-ms)]
                      (mpb/set-progress! prog-bar save-id (* 100 (double (/ n total))))

                      (swap! last-ms-elapsed-atom
                             assoc save-id elapsed))

                    (update-UI time-label prog-bar last-ms-elapsed-atom)))

                (fn []
                  (unregister-save! manager save-id)))]

     (save-image save-limits color-opt-str img))))

