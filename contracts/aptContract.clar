(impl-trait 'ST1VZ3YGJKKC8JSSWMS4EZDXXJM7QWRBEZ0ZWM64E.sip-010-trait-ft-standard.sip-010-trait)

(define-fungible-token TestStx u100000000000000)


(define-data-var tokenAdmin principal tx-sender)

(define-data-var token-uri (optional (string-utf8 30)) none)  ;; Storage for URI

(var-set token-uri (some u"https://mahan.gurung-tamu.com/"))


(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) (err u1))
    (ft-transfer? TestStx amount sender recipient)
  )
)

(define-read-only (get-balance (account principal))
  (ok (ft-get-balance TestStx account))
)

(define-public (mint (amount uint) (recipient principal) (nft-id uint))
  (begin
    (asserts! (is-eq tx-sender (var-get tokenAdmin)) (err u3))
    (is-ok (contract-call? .nft get-owner nft-id)) ;;nft-id must be fixed according to the asset so make it constant //nft get-ower must have assert! to check if the tx-sender is owner
    (ft-mint? TestStx amount recipient)
  )
)

(define-read-only (get-decimals)
    (ok u8)
)

(define-read-only (get-name) 
  (ok "Asset Property token")
)

(define-read-only (get-symbol) 
  (ok "APT")
)

(define-read-only (get-token-uri)
    (ok (var-get token-uri))  ;; Returns the stored URI or `none`
)

(define-read-only (get-total-supply) 
  (ok (ft-get-supply TestStx))
)