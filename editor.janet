(setdyn :pretty-format "%.40M")

(import freja/frp)
(use freja/defonce)
(use jaylib)
(use freja/collision)

(defonce editor @{})

(defn hsv->rgb
  [h s v]
  (def i (math/floor (* h 6)))
  (def f (- (* h 6) i))
  (def p (* v (- 1 s)))
  (def q (* v (- 1 (* f s))))
  (def t (* v (- 1 (* (- 1 f) s))))
  (def c (case (mod i 6)
           0 @[v t p]
           1 @[q v p]
           2 @[p v t]
           3 @[p q v]
           4 @[t p v]
           5 @[v p q])))

(comment
  (hsv->rgb 0.5 0.5 0.0)
  #
)

(defn relative-mouse-position
  [[x y]]
  (def [mx my] (get-mouse-position))
  [(math/floor (* 0.25 (- mx x)))
   (math/floor (* 0.25 (- my y)))])

(defn clear
  [{:canvas canvas}]
  (begin-texture-mode canvas)
  (clear-background :white)
  (end-texture-mode))

(defn draw
  [props dt]
  (def {:canvas canvas
        :canvas-rec canvas-rec
        :last-mp lmp} props)

  (def mp (relative-mouse-position canvas-rec))

  (begin-texture-mode canvas)
  (when (and (mouse-button-down? 0)
             (in-rec? (get-mouse-position)
                      canvas-rec))
    (if (= mp lmp)
      (draw-pixel ;mp (get props :color :red))
      (draw-line-ex lmp mp 1 (get props :color :red))))
  (end-texture-mode)

  (put props :last-mp mp)

  (rl-push-matrix)
  (rl-translatef (canvas-rec 0) (canvas-rec 1) 0)
  (draw-rectangle-rec [0 0 (canvas-rec 2) (canvas-rec 3)] :white)
  (-> (get-render-texture canvas)
      (draw-texture-pro
        [0 0 (* 0.25 (canvas-rec 2)) (* 0.25 (- (canvas-rec 3)))]
        [0 0 (canvas-rec 2) (canvas-rec 3)]
        [0 0]
        0
        :white))
  (rl-translatef 0 (canvas-rec 3) 0)
  (def mp (get-mouse-position))
  (def [mpx mpy]
    [(- (mp 0) (canvas-rec 0))
     (- (mp 1) (canvas-rec 1) (canvas-rec 3))])

  (when-let [c (props :color)]
    (draw-rectangle 100 0 50 50 c))

  (loop [s :range [0 100]
         v :range [0 100]
         :let [color (hsv->rgb (get props :hue 0.5) (* 0.01 s) (* 0.01 v))]]
    (when (and (mouse-button-down? 0)
               (= mpx s)
               (= mpy v))
      (put props :color color))
    (draw-pixel s v color
                #(hsv->rgb 0.3 0.2 0.6)
))

  (loop [s :range [0 300]
         :let [color (hsv->rgb (/ s 300) 1 1)]]
    (draw-line s 100 s 150 color))

  (when
    (and (mouse-button-down? 0)
         (> mpy 100))
    (put props :hue (min 1 (max 0 (/ mpx 300)))))

  (rl-pop-matrix)
  #
)

(def props
  # only create new render texture if none exists
  @{:canvas (or (editor :canvas)
                (load-render-texture 100 100))

    :canvas-rec [600 30 400 400]

    :on-event
    (fn [self ev]
      (match ev
        [:dt dt]
        (draw self dt)))})

#
#
#

(merge-into
  editor
  props)

(clear editor)

(frp/subscribe-finally! frp/frame-chan editor)
