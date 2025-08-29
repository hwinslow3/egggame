(module (egggame tiled-file) *

(import (scheme)
        (chicken base)
        (chicken string)
        (chicken sort)
        (chicken type)
        (srfi-1)
        (srfi-2)
        (srfi-13))

(define (sxml-tag-name sxml)
  (and (pair? sxml)
       (car sxml)))

(define (sxml-tag-has-attributes? sxml)
  (and (pair? sxml)
       (pair? (cdr sxml))
       (pair? (cadr sxml))
       (eq? '@ (car (cadr sxml)))))

(define (sxml-tag-attributes sxml)
  (or (and (sxml-tag-has-attributes? sxml)
           (cdr (cadr sxml)))
      '()))

(define (sxml-tag-attribute-ref sxml key)
  (and-let* ((pair (assoc key (sxml-tag-attributes sxml))))
    (cadr pair)))

(define (sxml-tag-body sxml)
  (or (and (pair? sxml)
           (sxml-tag-has-attributes? sxml)
           (cddr sxml))
      (and (pair? sxml)
           (not (sxml-tag-has-attributes? sxml))
           (cdr sxml))
      '()))

(define (sxml-toplevel-tag sxml)
  (and-let* ((_ (list? sxml))
             (_ (>= (length sxml) 3))
             (_ (eq? (car sxml) '*TOP*))
             (_ (and (pair? (cadr sxml))
                     (eq? (car (cadr sxml)) '*PI*))))
    (caddr sxml)))

(define (map-tilesets sxml)
  (filter (lambda (sxml) (eq? (sxml-tag-name sxml) 'tileset))
          (sxml-tag-body sxml)))

(define (tileset-images sxml)
  (filter (lambda (sxml) (eq? (sxml-tag-name sxml) 'image))
          (sxml-tag-body sxml)))

(define (tileset-source sxml)
  (let ((images (tileset-images sxml)))
    (unless (= 1 (length images))
      (error "wrong number of tileset images" images))
    (sxml-tag-attribute-ref (car images) 'source)))

(define (tileset-name sxml)
  (sxml-tag-attribute-ref sxml 'name))

(define (tileset-firstgid sxml)
  (string->number (sxml-tag-attribute-ref sxml 'firstgid)))

(define (tileset-columns sxml)
  (string->number (sxml-tag-attribute-ref sxml 'columns)))

(define (tileset-tilecount sxml)
  (string->number (sxml-tag-attribute-ref sxml 'tilecount)))

(define (tileset-tilewidth sxml)
  (string->number (sxml-tag-attribute-ref sxml 'tilewidth)))

(define (tileset-tileheight sxml)
  (string->number (sxml-tag-attribute-ref sxml 'tileheight)))

(define (tileset-tiledimensions sxml)
  (list (tileset-tilewidth sxml)
        (tileset-tileheight sxml)))

(define (map-layers sxml)
  (filter (lambda (sxml) (eq? (sxml-tag-name sxml) 'layer))
          (sxml-tag-body sxml)))

(define (map-layers/sorted sxml)
  (sort (map-layers sxml)
        (lambda (a b) (< (layer-id/number a) (layer-id/number b)))))

(define (layer-width sxml)
  (string->number (sxml-tag-attribute-ref sxml 'width)))

(define (layer-height sxml)
  (string->number (sxml-tag-attribute-ref sxml 'height)))

(define (layer-id sxml)
  (sxml-tag-attribute-ref sxml 'id))

(define (layer-id/number sxml)
  (string->number (layer-id sxml)))

(define (layer-datas sxml)
  (filter (lambda (data-sxml) (eq? (sxml-tag-name data-sxml) 'data))
          (sxml-tag-body sxml)))

(define (layer-data sxml)
  (let ((datas (layer-datas sxml)))
    (unless (= (length datas) 1)
      (error "too many data entries in layer" sxml))
    (unless (equal? (sxml-tag-attribute-ref (car datas) 'encoding) "csv")
      (error "not csv encoded data" sxml))
    (let ((data (car datas)))
      (chop
       (string-split (string-concatenate (filter string? (sxml-tag-body data)))
                     "\n,")
       (layer-width sxml)))))

(define (map-layers-depths sxml)
  ;; assuming near=0, far=-1
  (let ((count (+ 0.0 (length (map-layers sxml)))))
    ;; start is -0.8, step is 0.8/count
    (iota count (the number -0.8) (/ 0.8 count))))

)
