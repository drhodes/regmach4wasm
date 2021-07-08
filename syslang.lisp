;; ;;;; closforth.lisp

;; (in-package #:closforth)

;; (defgeneric eval-type (type-env expr) ())

;; ;; (eval-type)
;; ;; (type-env)

;; '(add 1 2 (mul 3 4 5))

;; (defun decl-struct-name (expr) (cadr expr))
;; (defun decl-struct-fields (expr) (caddr expr))
;; (defun field-type (field) (car field))
;; (defun field-name (field) (cadr field))

;; (field-name (car (decl-struct-fields
;;                   '(decl-struct stat ((short type)
;;                                       (int dev)
;;                                       (uint ino)
;;                                       (short nlink)
;;                                       (uint size))))))

;; (struct-alloc name type)


;; (defun eval-struct-decl (expr))
;; (defun typedef-name (expr) (cadr expr))
;; (defun typedef-type (expr) (caddr expr))
;; (defun eval-typedef (expr))

;; '( :heap (reserve 1000) )


;; '(;;
;;   (DIVC RA literal RC)
;;   (JMP RA RC)
;;   :kernel-write-msg
;;   ;;
;;   )


;; '(
;;   ;; in forth dot (.) pops and shows.  Lisp doesn't like reusing dot, so
;;   ;; how about $ for now.
  
;;   ;; build a language that can emit beta assembly.

;;   (typedef uint unsigned-int)
;;   (typedef ushort unsigned-short)
;;   (typedef uchar unsigned-char)
;;   (typedef pde_t uint)
;;   (typedef Align long)

;;   ;; struct stat {
;;   ;;   short type;
;;   ;;   int dev;
;;   ;;   uint ino;
;;   ;;   short nlink;
;;   ;;   uint size;
;;   ;; };


;;   (decl-struct stat ((short type)
;;                      (int dev)
;;                      (uint ino)
;;                      (short nlink)
;;                      (uint size)))

  


;;   ;; struct stat;

;;   (struct stat)

;;   ;; union header {
;;   ;;   struct {
;;   ;;     union header *ptr;
;;   ;;     uint size;
;;   ;;   } s;
;;   ;;   Align x;
;;   ;; };

;;   ;; typedef union header Header;

;;   ;; static Header base;
;;   ;; static Header *freep;
;;   (static header base)
;;   (static (header ptr) freep)



;;   ;; void
;;   ;; free(void *ap)
;;   ;; {
;;   ;;   Header *bp, *p;

;;   ;;   bp = (Header*)ap - 1;
;;   ;;   for(p = freep; !(bp > p && bp < p->s.ptr); p = p->s.ptr)
;;   ;;     if(p >= p->s.ptr && (bp > p || bp < p->s.ptr))
;;   ;;       break;
;;   ;;   if(bp + bp->s.size == p->s.ptr){
;;   ;;     bp->s.size += p->s.ptr->s.size;
;;   ;;     bp->s.ptr = p->s.ptr->s.ptr;
;;   ;;   } else
;;   ;;     bp->s.ptr = p->s.ptr;
;;   ;;   if(p + p->s.size == bp){
;;   ;;     p->s.size += bp->s.size;
;;   ;;     p->s.ptr = bp->s.ptr;
;;   ;;   } else
;;   ;;     p->s.ptr = bp;
;;   ;;   freep = p;
;;   ;; }






;;   ;;       break;
;;   ;;   if(bp + bp->s.size == p->s.ptr){
;;   ;;     bp->s.size += p->s.ptr->s.size;
;;   ;;     bp->s.ptr = p->s.ptr->s.ptr;
;;   ;;   } else
;;   ;;     bp->s.ptr = p->s.ptr;
;;   ;;   if(p + p->s.size == bp){
;;   ;;     p->s.size += bp->s.size;
;;   ;;     p->s.ptr = bp->s.ptr;
;;   ;;   } else
;;   ;;     p->s.ptr = bp;
;;   ;;   freep = p;
;;   ;; }
;;   (func free ((* void) ap) void
;;    (var (* header) bp)
;;    (var (* header) p)

;;    (set bp (ptr- (coerce (ptr header) ap) 1))


;;    ;; (while cond stmts)

;;    (while (and (> bp p) (< bp (sel (-> p s) ptr)))
;;     (when (and (>= p (sel (-> p s) ptr))
;;                (or (> bp p)
;;                    (< bp (sel (-> p s) ptr))))
;;       (break))
    
;;     ;; for loop update clause.
;;     (set p (sel (-> p s) ptr))
;;     )




;;    )








;;   ;; static Header*
;;   ;; morecore(uint nu)
;;   ;; {
;;   ;;   char *p;
;;   ;;   Header *hp;

;;   ;;   if(nu < 4096)
;;   ;;     nu = 4096;
;;   ;;   p = sbrk(nu * sizeof(Header));
;;   ;;   if(p == (char*)-1)
;;   ;;     return 0;
;;   ;;   hp = (Header*)p;
;;   ;;   hp->s.size = nu;
;;   ;;   free((void*)(hp + 1));
;;   ;;   return freep;
;;   ;; }


;;   ;; (func name args return-type stmts)

;;   (func morecore ((uint nu)) (static (ptr header))
;;    ;;
;;    (var (ptr char) p)
;;    (var (ptr header) hp)

;;    (when (< nu 4096)
;;      (set nu 4096))

;;    (set p (sbrk (* nu (sizeof header))))

;;    ;; (when cond stmts)
;;    (when (eq p (coerce -1 (ptr char)))
;;      (return 0))
   
;;    (set hp (coerce p (ptr header)))
;;    (set (sel (-> hp s) size) nu)
;;    (free (ptr+ hp 1))
   
;;    (return freep))




  



;;   ;; void*
;;   ;; malloc(uint nbytes)
;;   ;; {
;;   ;;   Header *p, *prevp;
;;   ;;   uint nunits;

;;   ;;   nunits = (nbytes + sizeof(Header) - 1)/sizeof(Header) + 1;
;;   ;;   if((prevp = freep) == 0){
;;   ;;     base.s.ptr = freep = prevp = &base;
;;   ;;     base.s.size = 0;
;;   ;;   }
;;   ;;   for(p = prevp->s.ptr; ; prevp = p, p = p->s.ptr){
;;   ;;     if(p->s.size >= nunits){
;;   ;;       if(p->s.size == nunits)
;;   ;;         prevp->s.ptr = p->s.ptr;
;;   ;;       else {
;;   ;;         p->s.size -= nunits;
;;   ;;         p += p->s.size;
;;   ;;         p->s.size = nunits;
;;   ;;       }
;;   ;;       freep = prevp;
;;   ;;       return (void*)(p + 1);
;;   ;;     }
;;   ;;     if(p == freep)
;;   ;;       if((p = morecore(nunits)) == 0)
;;   ;;         return 0;
;;   ;;   }
;;   ;; }

;;   '((func malloc (((uint32 nbytes)) void*)      
;;      (var (ptr header) p)
;;      (var (ptr header) prevp)
;;      (var uint nunits)
     
;;      ;; nunits = (nbytes + sizeof(Header) - 1)/sizeof(Header) + 1;
;;      (set nunits (+ 1 (/ (- (+ nbytes (sizeof header)) 1)
;;                        (sizeof header))))

;;      (set p (sel (-> prevp s) ptr)) ;; p = prevp->s.ptr;

;;      (set prevp freep)
;;      (if (nullptr? prevp)
;;          (progn
;;            (set (prevp (& base)))
;;            (set (freep (& base)))            
;;            (set (sel base s ptr) (& base))))
     
     
;;      ;;   for(p = prevp->s.ptr; ; prevp = p, p = p->s.ptr){
;;      (forever
;;       ;;     if(p->s.size >= nunits){
;;       (if (>= (p-> (sel s size) nunits))
;;           (set (prevp-> (sel s ptr)) (sel (-> p s) ptr))
;;           (progn
;;             ;; p->s.size -= nunits;
;;             ;; p += p->s.size;
;;             ;; p->s.size = nunits;
;;             (set p (- p (sel (-> p s) size) nunits))
;;             (set p (+ p (sel (-> p s) size)))
;;             (set (sel (-> p s) size) nunits))
;;           ;; freep = prevp;
;;           ;; return (void*)(p + 1);
;;           ;; }
;;           ;;     if(p == freep)
;;           ;;       if((p = morecore(nunits)) == 0)
;;           ;;         return 0;
;;           ;;   }

;;           (if (eq p freep)
;;               (set p (morecore nunits))
;;               (when (zero? p) (return 0))))

;;       ;; END OF FOR LOOP:  prevp = p, p = p->s.ptr
;;       (set prevp p)
;;       (set p (p-> (member s ptr)))

      
;;       )))




;;   ;; int fork(void);
;;   ;; int exit(void) __attribute__((noreturn));
;;   ;; int wait(void);
;;   ;; int pipe(int*);
;;   ;; int write(int, void*, int);
;;   ;; int read(int, void*, int);
;;   ;; int close(int);
;;   ;; int kill(int);
;;   ;; int exec(char*, char**);
;;   ;; int open(char*, int);
;;   ;; int mknod(char*, short, short);
;;   ;; int unlink(char*);
;;   ;; int fstat(int fd, struct stat*);
;;   ;; int link(char*, char*);
;;   ;; int mkdir(char*);
;;   ;; int chdir(char*);
;;   ;; int dup(int);
;;   ;; int getpid(void);
;;   ;; char* sbrk(int);
;;   ;; int sleep(int);
;;   ;; int uptime(void);


;;   ;; int stat(char*, struct stat*);
;;   ;; char* strcpy(char*, char*);
;;   ;; void *memmove(void*, void*, int);
;;   ;; char* strchr(const char*, char c);
;;   ;; int strcmp(const char*, const char*);
;;   ;; void printf(int, char*, ...);
;;   ;; char* gets(char*, int max);
;;   ;; uint strlen(char*);
;;   ;; void* memset(void*, int, uint);
;;   ;; void* malloc(uint);
;;   ;; void free(void*);
;;   ;; int atoi(const char*);
;;   ;; # 4 "umalloc.c" 2
;;   ;; # 1 "param.h" 1
;;   ;; # 5 "umalloc.c" 2
;;   )

