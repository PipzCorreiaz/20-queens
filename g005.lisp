;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   GRUPO 5  -  69930  -  Filipa Correia    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun resolve-problema (estado-inicial tipo-procura)
  (let* ((problema (cria-problema
                     (cria-estado estado-inicial)
                     (cons #'gera-sucessores '())
                     :objectivo? #'f-objectivo?
                     :heuristica #'h2))
         (resultado (procura problema tipo-procura)))
    (processa-resultado resultado)))


;;; A representacao para o estado que escolhi e um array
;;; em que cada index representa a linha e o conteudo a
;;; coluna da rainha se nao houver rainha e NIL
(defun cria-estado (estado)
  (let* ((dim (array-dimension estado 0))
         (l (make-array dim)))
    (dotimes (i dim)
      (dotimes (j dim)
        (when (pos-rainha? estado i j)
          (setf (aref l i) j)
          (return))))
    l))


;;; verifa se o objetivo de colocar todas
;;; as rainhas ja foi atingido
(defun f-objectivo? (estado)
  (dotimes (linha (length estado))
    (when (linha-livre? estado linha)
      (return-from f-objectivo? NIL)))
  T)


;;; procura a primeira linha livre e executa
;;; a funcao rainhas-possiveis para essa linha
(defun gera-sucessores (estado)
  (dotimes (linha (length estado))
    (when (linha-livre? estado linha)
      (return-from gera-sucessores
                   (rainhas-possiveis-linha estado linha)))))


;;; para todas as colunas de uma dada linha
;;; verifica se nao ha conflitos e devolve
;;; a lista com os estados possiveis nessa linha
(defun rainhas-possiveis-linha (estado linha)
  (let ((dim (length estado)))
    (labels ((aux (coluna)
                  (if (= coluna dim)
                      '()
                      (if (rainha-valida? estado linha coluna)
                          (let ((copia-estado (copy-array estado)))
                            (setf (aref copia-estado linha) coluna)
                            (cons copia-estado (aux (+ 1 coluna))))
                          (aux (+ 1 coluna))))))
            (aux 0))))


;;; para uma dada posicao verifica as colisoes
;;; com as rainhas ja colocadas
(defun rainha-valida? (estado l c)
  (dotimes (linha (length estado))
    (let ((coluna (aref estado linha)))
      (when (not (eq coluna NIL))
        (let ((r1 (cons linha coluna))
              (r2 (cons l c)))
          (when (rainhas-conflito? r1 r2)
            (return-from rainha-valida? NIL))))))
  T)


;;; verifica se duas rainhas estao em conflito
(defun rainhas-conflito? (r1 r2)
  (or (= (car r1) (car r2))
      (= (cdr r1) (cdr r2))
      (= (abs (- (car r1) (car r2)))
         (abs (- (cdr r1) (cdr r2))))))


;;; verifica se uma dada posicao do
;;; tabuleiro inicial contem uma rainha
(defun pos-rainha? (tab l c)
  (aref tab l c))


;;; verifica se uma linha nao tem rainhas
(defun linha-livre? (estado l)
  (eq (aref estado l) NIL))


;;; funcao que processa o resultado de modo
;;; a obter o output correcto
(defun processa-resultado (res)
  (labels ((estado-final (lista)
                         (if (eq (cdr lista) NIL)
                             (array-matriz (car lista))
                             (estado-final (cdr lista))))
           (array-matriz (array)
                         (let* ((dim (length array))
                                (r (make-array (list dim dim))))
                           (dotimes (linha dim)
                             (let ((coluna (aref array linha)))
                               (setf (aref r linha coluna) T)))
                           r)))
          (if (eq (car res) NIL)
              NIL
              (estado-final (car res)))))


;;; NAO USEI ESTA HEURISTICA pois h2 e melhor

;;; multiplica o numero de posicoes com conflitos
;;; em cada linha livre por um factor que prejudica
;;; o nr de conflitos das ultimas linhas livres
;;; devolve a soma desses conflitos enfatizados
;;; pela linha a multiplicar pelo numero de
;;; rainhas que faltam colocar
(defun h1 (estado)
  (let ((dim (length estado))
        (rainhas-que-faltam 0)
        (res 0)
        (factor 1))
    (dotimes (i dim)
      (when (linha-livre? estado i)
        (let ((conflitos 0))
          (setf rainhas-que-faltam (+ rainhas-que-faltam 1))
          (dotimes (j dim)
            (let ((r2 (cons i j)))
              (dotimes (k dim)
                (when (and (not (eq (aref estado k) NIL))
                           (rainhas-conflito? (cons k (aref estado k)) r2))
                  (setf conflitos (+ conflitos 1))))))
          (setf res (+ res (* conflitos factor)))
          (setf factor (+ factor 1)))))
    
    (* res rainhas-que-faltam)))


;;; HEURISTICA UTILIZADA

;;; a funcao calc-n calcula quantos conflitos
;;; uma rainha causa isoladamente (indepente das
;;; ja colocadas)
;;; a heuristica devolve o numero de rainhas que
;;; que faltam colocar vezes a media do nr de conflitos
;;; que as rainhas ja colocadas causam
(defun h2 (estado)
  (let ((dim (length estado))
        (count-rainhas 0)
        (sum-n 0))
    (labels ((calc-n (l c)
                     (let ((n (- dim 1)))
                       (+ (* n 3) (min l c (- n l) (- n c))))))
            (dotimes (i dim)
              (when (not (eq (aref estado i) NIL))
                (setf count-rainhas (+ count-rainhas 1))
                (setf sum-n (+ sum-n (calc-n i (aref estado i))))))
            (if (= count-rainhas 0)
                (* (- dim count-rainhas) (calc-n 0 0))
                (* (- dim count-rainhas) (/ sum-n count-rainhas))))))



