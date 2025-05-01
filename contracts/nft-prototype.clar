;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FTFactory.clar (Clarity v3)
;; A factory that can issue & manage multiple fungible-token types,
;; ensuring unique names and symbols per token.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1) Import the SIP-009 fungible-token trait
(use-trait fungible-token 'ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.sip-010-trait-ft-standard.sip-010-trait)

;; ----------------------------------------------------------------------------
;; STORAGE
;; ----------------------------------------------------------------------------
(define-data-var token-counter uint u0)
(define-data-var contract-owner principal tx-sender)

(define-map token-metadata
  { id: uint }
  { name: (buff 32), symbol: (buff 8), decimals: uint, total-supply: uint })

(define-map name-to-id
  { name: (buff 32) }
  { id: uint })

(define-map symbol-to-id
  { symbol: (buff 8) }
  { id: uint })

(define-map balances
  { id: uint, owner: principal }
  { balance: uint })

;; ----------------------------------------------------------------------------
;; FACTORY: create a new FT type with unique name & symbol, using trait mint
;; ----------------------------------------------------------------------------
(define-public (create-token (name (buff 32)) (symbol (buff 8)) (decimals uint) (initial uint))
  (begin
    ;; Ensure unique name & symbol
    (asserts! (is-none (map-get? name-to-id { name: name })) (err u105))
    (asserts! (is-none (map-get? symbol-to-id { symbol: symbol })) (err u106))
    ;; Assign new token-id
    (let ((new-id (var-get token-counter)))
      ;; Increment counter
      (var-set token-counter (+ new-id u1))
      ;; Store metadata & total-supply
      (map-set token-metadata { id: new-id }
               { name: name, symbol: symbol, decimals: decimals, total-supply: initial })
      ;; Record uniqueness mappings
      (map-set name-to-id   { name: name }   { id: new-id })
      (map-set symbol-to-id { symbol: symbol } { id: new-id })
      ;; Mint initial supply to creator via trait
      (ft-mint? new-id initial tx-sender)
      ;; Return the token-id
      (ok new-id))))

;; ----------------------------------------------------------------------------
;; TRAIT IMPLEMENTATIONS (SIP-009) using match-opt
;; ----------------------------------------------------------------------------

(define-read-only (ft-get-name (id uint))
  (match (map-get? token-metadata { id: id })
    md  (ok (get name md))
    (err u100)))

(define-read-only (ft-get-symbol (id uint))
  (match (map-get? token-metadata { id: id })
    md  (ok (get symbol md))
    (err u101)))

(define-read-only (ft-get-decimals (id uint))
  (match (map-get? token-metadata { id: id })
    md  (ok (get decimals md))
    (err u102)))

(define-read-only (ft-get-total-supply (id uint))
  (match (map-get? token-metadata { id: id })
    md  (ok (get total-supply md))
    (err u103)))

(define-read-only (ft-get-balance (id uint) (owner principal))
  (default-to u0 (get balance (map-get? balances { id: id, owner: owner }))))

(define-public (ft-transfer? (id uint) (amount uint) (sender principal) (recipient principal))
  (let ((bal-sender (ft-get-balance id sender))
        (bal-rec    (ft-get-balance id recipient)))
    (asserts! (>= bal-sender amount) (err u200))
    (map-set balances { id: id, owner: sender }    { balance: (- bal-sender amount) })
    (map-set balances { id: id, owner: recipient } { balance: (+ bal-rec amount) })
    (ok true)))

;; ----------------------------------------------------------------------------
;; OPTIONAL: Mint & Burn (owner-only), named with '?'
;; ----------------------------------------------------------------------------
(define-private (is-owner)
  (ok (is-eq tx-sender (var-get contract-owner))))

(define-public (ft-mint? (id uint) (amount uint) (recipient principal))
  (begin
    (asserts! (unwrap-panic (is-owner)) (err u300))
    (let ((bal (ft-get-balance id recipient)))
      (match (map-get? token-metadata { id: id })
        md
          (begin
            (map-set balances       { id: id, owner: recipient } { balance: (+ bal amount) })
            (map-set token-metadata { id: id }
                     { name: (get name md)
                     , symbol: (get symbol md)
                     , decimals: (get decimals md)
                     , total-supply: (+ (get total-supply md) amount) })
            (ok true))
        (err u301)))))

(define-public (ft-burn? (id uint) (amount uint) (from principal))
  (begin
    (asserts! (unwrap-panic (is-owner)) (err u302))
    (let ((bal (ft-get-balance id from)))
      (asserts! (>= bal amount) (err u303))
      (match (map-get? token-metadata { id: id })
        md
          (begin
            (map-set balances       { id: id, owner: from }    { balance: (- bal amount) })
            (map-set token-metadata { id: id }
                     { name: (get name md)
                     , symbol: (get symbol md)
                     , decimals: (get decimals md)
                     , total-supply: (- (get total-supply md) amount) })
            (ok true))
        (err u304)))))

;; ----------------------------------------------------------------------------
;; HELPER: token count
;; ----------------------------------------------------------------------------
(define-read-only (get-token-count) (ok (var-get token-counter)))
