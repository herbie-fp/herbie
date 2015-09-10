
(herbie-test ([m00 double] [m10 double] [m20 double] [m30 double]
              [m01 double] [m11 double] [m21 double] [m31 double]
              [m02 double] [m12 double] [m22 double] [m32 double]
              [m03 double] [m13 double] [m23 double] [m33 double]

              [b0 double]
              [b1 double]
              [b2 double]
              [b3 double]

              [x0_0 double]
              [x0_1 double]
              [x0_2 double]
              [x0_3 double]

              [max_iters (integer-range 2 100)])
  "HPCCG Mini-App from Sandia Mini-apps"
  (let* ([mx0 (+ (* x0_0 m00) (* x0_1 m10) (* x0_2 m20) (* x0_3 m30))]
         [mx1 (+ (* x0_0 m01) (* x0_1 m11) (* x0_2 m21) (* x0_3 m31))]
         [mx2 (+ (* x0_0 m02) (* x0_1 m12) (* x0_2 m22) (* x0_3 m32))]
         [mx3 (+ (* x0_0 m03) (* x0_1 m13) (* x0_2 m23) (* x0_3 m33))]
         [init-r0 (- mx0 b0)]
         [init-r1 (- mx1 b1)]
         [init-r2 (- mx2 b2)]
         [init-r3 (- mx3 b3)])
    (do ([r0 init-r0
             (let* ([mp0 (+ (* p0 m00) (* p1 m10) (* p2 m20) (* p3 m30))]
                    [mp1 (+ (* p0 m01) (* p1 m11) (* p2 m21) (* p3 m31))]
                    [mp2 (+ (* p0 m02) (* p1 m12) (* p2 m22) (* p3 m32))]
                    [mp3 (+ (* p0 m03) (* p1 m13) (* p2 m23) (* p3 m33))]

                    [alpha0 (+ (* p0 mp0) (* p1 mp1) (* p2 mp2) (* p3 mp3))]
                    [alpha1 (/ (+ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3))
                               alpha0)])
               (- r0 (* alpha1 mp0)))]
         [r1 init-r1
             (let* ([mp0 (+ (* p0 m00) (* p1 m10) (* p2 m20) (* p3 m30))]
                    [mp1 (+ (* p0 m01) (* p1 m11) (* p2 m21) (* p3 m31))]
                    [mp2 (+ (* p0 m02) (* p1 m12) (* p2 m22) (* p3 m32))]
                    [mp3 (+ (* p0 m03) (* p1 m13) (* p2 m23) (* p3 m33))]

                    [alpha0 (+ (* p0 mp0) (* p1 mp1) (* p2 mp2) (* p3 mp3))]
                    [alpha1 (/ (+ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3))
                               alpha0)])
               (- r1 (* alpha1 mp1)))]
         [r2 init-r2
             (let* ([mp0 (+ (* p0 m00) (* p1 m10) (* p2 m20) (* p3 m30))]
                    [mp1 (+ (* p0 m01) (* p1 m11) (* p2 m21) (* p3 m31))]
                    [mp2 (+ (* p0 m02) (* p1 m12) (* p2 m22) (* p3 m32))]
                    [mp3 (+ (* p0 m03) (* p1 m13) (* p2 m23) (* p3 m33))]

                    [alpha0 (+ (* p0 mp0) (* p1 mp1) (* p2 mp2) (* p3 mp3))]
                    [alpha1 (/ (+ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3))
                               alpha0)])
               (- r2 (* alpha1 mp2)))]
         [r3 init-r3
             (let* ([mp0 (+ (* p0 m00) (* p1 m10) (* p2 m20) (* p3 m30))]
                    [mp1 (+ (* p0 m01) (* p1 m11) (* p2 m21) (* p3 m31))]
                    [mp2 (+ (* p0 m02) (* p1 m12) (* p2 m22) (* p3 m32))]
                    [mp3 (+ (* p0 m03) (* p1 m13) (* p2 m23) (* p3 m33))]

                    [alpha0 (+ (* p0 mp0) (* p1 mp1) (* p2 mp2) (* p3 mp3))]
                    [alpha1 (/ (+ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3))
                               alpha0)])
               (- r3 (* alpha1 mp3)))]

         [r-sqrlen (+ (* init-r0 init-r0) (* init-r1 init-r1) (* init-r2 init-r2) (* init-r3 init-r3))
                   (+ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3))]

         [p0 init-r0
             (if (= k 1) init-r0
                 (let ([beta (/ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3)
                                r-sqrlen)])
                   (+ r0 (* beta p0))))]
         [p1 init-r1
             (if (= k 1) init-r1
                 (let ([beta (/ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3)
                                r-sqrlen)])
                   (+ r1 (* beta p1))))]
         [p2 init-r2
             (if (= k 1) init-r2
                 (let ([beta (/ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3)
                                r-sqrlen)])
                   (+ r2 (* beta p2))))]
         [p3 init-r3
             (if (= k 1) init-r3
                 (let ([beta (/ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3)
                                r-sqrlen)])
                   (+ r3 (* beta p3))))]

         [x0 x0_0
             (let* ([mp0 (+ (* p0 m00) (* p1 m10) (* p2 m20) (* p3 m30))]
                    [mp1 (+ (* p0 m01) (* p1 m11) (* p2 m21) (* p3 m31))]
                    [mp2 (+ (* p0 m02) (* p1 m12) (* p2 m22) (* p3 m32))]
                    [mp3 (+ (* p0 m03) (* p1 m13) (* p2 m23) (* p3 m33))]

                    [alpha0 (+ (* p0 mp0) (* p1 mp1) (* p2 mp2) (* p3 mp3))]
                    [alpha1 (/ (+ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3))
                               alpha0)])
               (+ x0 (* alpha1 p0)))]
         [x1 x0_1
             (let* ([mp0 (+ (* p0 m00) (* p1 m10) (* p2 m20) (* p3 m30))]
                    [mp1 (+ (* p0 m01) (* p1 m11) (* p2 m21) (* p3 m31))]
                    [mp2 (+ (* p0 m02) (* p1 m12) (* p2 m22) (* p3 m32))]
                    [mp3 (+ (* p0 m03) (* p1 m13) (* p2 m23) (* p3 m33))]

                    [alpha0 (+ (* p0 mp0) (* p1 mp1) (* p2 mp2) (* p3 mp3))]
                    [alpha1 (/ (+ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3))
                               alpha0)])
               (+ x1 (* alpha1 p1)))]
         [x2 x0_2
             (let* ([mp0 (+ (* p0 m00) (* p1 m10) (* p2 m20) (* p3 m30))]
                    [mp1 (+ (* p0 m01) (* p1 m11) (* p2 m21) (* p3 m31))]
                    [mp2 (+ (* p0 m02) (* p1 m12) (* p2 m22) (* p3 m32))]
                    [mp3 (+ (* p0 m03) (* p1 m13) (* p2 m23) (* p3 m33))]

                    [alpha0 (+ (* p0 mp0) (* p1 mp1) (* p2 mp2) (* p3 mp3))]
                    [alpha1 (/ (+ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3))
                               alpha0)])
               (+ x2 (* alpha1 p2)))]
         [x3 x0_3
             (let* ([mp0 (+ (* p0 m00) (* p1 m10) (* p2 m20) (* p3 m30))]
                    [mp1 (+ (* p0 m01) (* p1 m11) (* p2 m21) (* p3 m31))]
                    [mp2 (+ (* p0 m02) (* p1 m12) (* p2 m22) (* p3 m32))]
                    [mp3 (+ (* p0 m03) (* p1 m13) (* p2 m23) (* p3 m33))]

                    [alpha0 (+ (* p0 mp0) (* p1 mp1) (* p2 mp2) (* p3 mp3))]
                    [alpha1 (/ (+ (* r0 r0) (* r1 r1) (* r2 r2) (* r3 r3))
                               alpha0)])
               (+ x3 (* alpha1 p3)))]

         [k 1 (+ k 1)])
        (< k max_iters)
      ;; For now we're just returning the first element of the
      ;; vector here, since they all affect each other, but the
      ;; real output is supposed to be the whole vector.
      x0)))
        
