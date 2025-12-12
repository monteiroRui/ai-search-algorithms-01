;;;; projeto.lisp
;;;; Projeto01 IA - 25/26
;;;; Requer: puzzle.lisp, procura.lisp, problemas.dat

(defparameter *base-path*
  (or *load-pathname*
      *compile-file-pathname*
      (error "Nao foi possivel determinar o caminho base.")))

(load (merge-pathnames "puzzle.lisp" *base-path*))
(load (merge-pathnames "procura.lisp" *base-path*))

(defun ler-problemas (ficheiro)
  (with-open-file (f ficheiro :direction :input)
    (let ((problemas '())
          (acc ""))
      (loop for linha = (read-line f nil 'eof)
            until (eq linha 'eof) do
              (cond
                ((string= linha "###")
                 (when (plusp (length (string-trim " " acc)))
                   (push (read-from-string acc) problemas))
                 (setf acc ""))
                (t
                 (setf acc
                       (concatenate 'string acc linha (string #\Newline))))))
      (when (plusp (length (string-trim " " acc)))
        (push (read-from-string acc) problemas))
      (nreverse problemas))))

(defun imprime-tabuleiro (tab)
  (format t "~%---------------- Tabuleiro ----------------~%")
  (dolist (linha tab)
    (dolist (c linha)
      (format t "~A "
              (cond
                ((null c) " ")
                ((= c 1) "1")
                ((= c 0) "0"))))
    (format t "~%"))
  (format t "---------------------------------------------~%"))

(defun menu-problema (lista)
  (format t "~%=========== SELECIONE O PROBLEMA ===========~%")
  (loop for i from 1 to (length lista) do
        (format t "~A - Problema ~A~%" i i))
  (format t "Escolha: ")
  (let ((n (read)))
    (values n (nth (1- n) lista))))

(defun menu-algoritmo ()
  (format t "~%=========== SELECIONE O ALGORITMO ==========~%")
  (format t "1 - BFS (Procura em Largura)~%")
  (format t "2 - DFS (Procura em Profundidade)~%")
  (format t "Escolha: ")
  (case (read)
    (1 'bfs)
    (2 'dfs)
    (t (menu-algoritmo))))

(defun mostrar-caminho (no-final)
  (let ((caminho (construir-caminho no-final)))
    (format t "~%========= SOLUCAO ENCONTRADA =========~%")
    (format t "Profundidade da solucao: ~A~%" (no-g no-final))
    (format t "Numero de passos: ~A~%" (length caminho))
    (format t "~%===== Caminho (estado por estado) =====~%")
    (dolist (estado caminho)
      (imprime-tabuleiro estado))
    (format t "~%========================================~%")
    no-final))

(defun limpar-relatorio (ficheiro)
  (with-open-file (f ficheiro :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (format f "===== RELATORIO DE DESEMPENHO - PEG SOLITAIRE =====~%")))

(defun escrever-relatorio (ficheiro problema-id algoritmo resultado
                           nos-gerados nos-expandidos bmedio penetrancia tempo)
  (with-open-file (f ficheiro :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format f "~%==============================~%")
    (format f "Problema: ~A~%" problema-id)
    (format f "Algoritmo: ~A~%" algoritmo)
    (format f "Solucao: ~A~%" (if resultado "ENCONTRADA" "NAO ENCONTRADA"))
    (when resultado
      (format f "Profundidade: ~A~%" (no-g resultado)))
    (format f "Nos gerados: ~A~%" nos-gerados)
    (format f "Nos expandidos: ~A~%" nos-expandidos)
    (format f "Fator ramificacao medio: ~,4F~%" bmedio)
    (format f "Penetrancia: ~,6F~%" penetrancia)
    (format f "Tempo (s): ~,4F~%" tempo)))

(defun iniciar ()
  (format t "~%===== PROJETO 01 - PEG SOLITAIRE SOLVER =====~%")
  (format t "~%A carregar problemas...~%")
  (let* ((problemas (ler-problemas (merge-pathnames "problemas.dat" *base-path*)))
         (relatorio (merge-pathnames "resultados.txt" *base-path*)))
    (limpar-relatorio relatorio)
    (multiple-value-bind (problema-id problema-escolhido)
        (menu-problema problemas)
      (let ((algoritmo (menu-algoritmo)))
        (format t "~%Problema escolhido:~%")
        (imprime-tabuleiro problema-escolhido)
        (cond
          ((eq algoritmo 'bfs)
           (format t "~%Executando BFS...~%")
           (multiple-value-bind (resultado g e b p tempo)
               (bfs problema-escolhido #'objetivo? #'gera-sucessores)
             (escrever-relatorio relatorio problema-id "BFS" resultado g e b p tempo)
             (if resultado
                 (mostrar-caminho resultado)
                 (format t "~%NAO FOI ENCONTRADA SOLUCAO.~%"))))
          ((eq algoritmo 'dfs)
           (format t "~%Qual a profundidade limite? ")
           (let ((lim (read)))
             (format t "~%Executando DFS com limite ~A...~%" lim)
             (multiple-value-bind (resultado g e b p tempo)
                 (dfs problema-escolhido #'objetivo? #'gera-sucessores lim)
               (escrever-relatorio relatorio problema-id
                                   (format nil "DFS(lim=~A)" lim)
                                   resultado g e b p tempo)
               (if resultado
                   (mostrar-caminho resultado)
                   (format t "~%NAO FOI ENCONTRADA SOLUCAO.~%")))))
          (t
           (format t "~%Algoritmo invalido.~%")))))))
