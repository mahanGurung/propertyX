;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FTFactory.clar
;; A "factory" that can issue & manage multiple fungible-token types,
;; ensuring unique names and symbols per token.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1) Use the SIP-009 fungible-token trait
(use-trait fungible-token
  .ft-trait.fungible-token-ft)

;; ----------------------------------------------------------------------------
;; STORAGE
;; ----------------------------------------------------------------------------
;; Counter for next token-id
(define-data-var token-counter uint u0)

;; Map: token-id => { name: (buff 32), symbol: (buff 8), decimals: uint, total-supply: uint }
(define-map token-metadata
  ((id uint))
  ((name (buff 32))
   (symbol (buff 8))
   (decimals uint)
   (total-supply uint)))

;; Map for uniqueness checks: name => id
(define-map name-to-id
  ((name (buff 32)))
  ((id uint)))

;; Map for uniqueness checks: symbol => id
(define-map symbol-to-id
  ((symbol (buff 8)))
  ((id uint)))

;; Map: (token-id, owner)  balance
(define-map balances
  ((id uint) (owner principal))
  ((balance uint)))

;; ----------------------------------------------------------------------------
;; FACTORY: create a new FT type with unique name & symbol
;; ----------------------------------------------------------------------------
(define-public (create-token (name (buff 32)) (symbol (buff 8)) (decimals uint) (initial uint))
  (begin
    ;; Ensure name is unique
    (let ((exists-name (map-get? name-to-id { name: name })))
      (asserts! (is-none exists-name) (err u105)))
    ;; Ensure symbol is unique
    (let ((exists-sym (map-get? symbol-to-id { symbol: symbol })))
      (asserts! (is-none exists-sym) (err u106)))
    
    ;; Compute new token-id
    (let ((new-id (var-get token-counter))
          (caller tx-sender))
      ;; Increment counter
      (var-set token-counter (+ new-id u1))
      ;; Record metadata & total-supply
      (map-set token-metadata
        { id: new-id }
        { name: name
        , symbol: symbol
        , decimals: decimals
        , total-supply: initial })
      ;; Register name & symbol for uniqueness
      (map-set name-to-id { name: name } { id: new-id })
      (map-set symbol-to-id { symbol: symbol } { id: new-id })
      ;; Mint initial balance to caller
      (map-set balances
        { id: new-id, owner: caller }
        { balance: initial })
      (ok new-id))))

;; ----------------------------------------------------------------------------
;; IMPLEMENT SIP-009 TRAIT
;; ----------------------------------------------------------------------------

(define-read-only (ft-get-name (id uint))
  (match (map-get token-metadata { id: id })
    metadata (ok (get name metadata))
    (err u100)))

(define-read-only (ft-get-symbol (id uint))
  (match (map-get token-metadata { id: id })
    metadata (ok (get symbol metadata))
    (err u101)))

(define-read-only (ft-get-decimals (id uint))
  (match (map-get token-metadata { id: id })
    metadata (ok (get decimals metadata))
    (err u102)))

(define-read-only (ft-get-total-supply (id uint))
  (match (map-get token-metadata { id: id })
    metadata (ok (get total-supply metadata))
    (err u103)))

(define-read-only (ft-get-balance (id uint) (owner principal))
  (default-to u0 (get balance (map-get? balances { id: id, owner: owner }))))

(define-public (ft-transfer (id uint) (amount uint) (sender principal) (recipient principal))
  (begin
    (let ((bal-sender (ft-get-balance id sender))
          (bal-rec   (ft-get-balance id recipient)))
      (asserts! (>= bal-sender amount) (err u200))
      (map-set balances { id: id, owner: sender }
               { balance: (- bal-sender amount) })
      (map-set balances { id: id, owner: recipient }
               { balance: (+ bal-rec amount) })
      (ok true))))

;; ----------------------------------------------------------------------------
;; OPTIONAL MINT & BURN (OWNER ONLY)
;; ----------------------------------------------------------------------------
(define-private (is-owner)
  (ok (is-eq tx-sender (contract-owner))))

(define-public (ft-mint (id uint) (amount uint) (recipient principal))
  (begin
    (asserts! (unwrap-panic (is-owner)) (err u300))
    (let ((bal   (ft-get-balance id recipient))
          (meta  (unwrap-panic (map-get token-metadata {id: id}))))
      (map-set balances { id: id, owner: recipient }
               { balance: (+ bal amount) })
      (map-set token-metadata { id: id }
               { name:     (get name meta)
               , symbol:   (get symbol meta)
               , decimals: (get decimals meta)
               , total-supply: (+ (get total-supply meta) amount) })
      (ok true))))

(define-public (ft-burn (id uint) (amount uint) (from principal))
  (begin
    (asserts! (unwrap-panic (is-owner)) (err u301))
    (let ((bal   (ft-get-balance id from))
          (meta  (unwrap-panic (map-get token-metadata {id: id}))))
      (asserts! (>= bal amount) (err u302))
      (map-set balances { id: id, owner: from }
               { balance: (- bal amount) })
      (map-set token-metadata { id: id }
               { name:     (get name meta)
               , symbol:   (get symbol meta)
               , decimals: (get decimals meta)
               , total-supply: (- (get total-supply meta) amount) })
      (ok true))))

;; ----------------------------------------------------------------------------
;; HELPER: see how many tokens exist
;; ----------------------------------------------------------------------------
(define-read-only (get-token-count)
  (ok (var-get token-counter)))
