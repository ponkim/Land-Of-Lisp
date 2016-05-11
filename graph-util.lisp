(defparameter *wizard-nodes* `((living-room (you are in the living-room.))
                        (garden (you are in a beautiful garden.))
                        (attic (you are in the attic.))))

;エッジ(移動手段)
(defparameter *wizard-edges* `((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))
;識別子の変換
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

;ノードをDOTフォーマットへ変換
(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

;エッジをDOTフォーマットへ変換
(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

;グラフをDOTフォーマットへ変換
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

;標準出力に出力される内容をDOTファイルとして書き込みグラフ画像を生成する
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O "fname)))

;グラフを画像にする
(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

;無向グラフを追加
(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst)) ;残りのリストに行き先ノードが無いとき
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))
