
;; title: rws-prototype
;; version:
;; summary:
;; description:

;; traits
;;
(impl-trait 'ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.sip-010-trait-ft-standard.sip-010-trait)
(use-trait fungible-token 'ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.sip-010-trait-ft-standard.sip-010-trait)


;; token definitions
;;
(define-fungible-token PXT u10000)


;; constants
;;

;; data vars
;;
(define-data-var tokenAdmin principal tx-sender)

(define-data-var token-uri (optional (string-utf8 30)) none)  ;; Storage for URI

(var-set token-uri (some u"https://mahan.gurung-tamu.com/"))

;; data maps
;;
(define-map AptForApproval { assetOwner: principal, assetTokeniztionRequest: uint } { aptName: (string-utf8 8), aptAmount: uint, ipfsData: (string-utf8 120), allowTokenization: bool })
(define-map TotalAssetsOfUser { User: principal } { totalAssets: uint })
(define-map Assets { owner: principal, AssetNo: uint } { name: (string-utf8 120), amount: uint, ipfsData: (string-utf8 120), contractAdd: principal, processCompleted: bool })
(define-map KYC { User: principal } { processCompleted: bool, ipfsData: (string-utf8 120) })

;; public functions
;;

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) (err u1))
    
    (ft-transfer? PXT amount sender recipient)
  )
)



(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (var-get tokenAdmin)) (err u3))
    (ft-mint? PXT amount recipient)
  )
)

(define-public (completeTokenization (assetOwner principal) (assetNo uint))
  (let ((asset (unwrap! (map-get? Assets {owner: assetOwner, AssetNo: assetNo})
                        (err u404))))
    (ok (map-set Assets 
            {owner: assetOwner, AssetNo: assetNo}
            {name: (get name asset),
            amount: (get amount asset),
            ipfsData: (get ipfsData asset),
            contractAdd: (get contractAdd asset),
            processCompleted: true}
      )
    )
  )
)


;; read only functions
;;
(define-read-only (get-balance (account principal))
  (ok (ft-get-balance PXT account))
)


(define-read-only (get-decimals)
    (ok u8)
)

(define-read-only (get-name) 
  (ok "PXT")
)

(define-read-only (get-symbol) 
  (ok "tStx")
)

(define-read-only (get-token-uri)
    (ok (var-get token-uri))  ;; Returns the stored URI or `none`
)

(define-read-only (get-total-supply) 
  (ok (ft-get-supply PXT))
) 

   
(define-read-only (aptMint (assetOwner principal) (assetNo uint))
  (let ((kyc-data (map-get? KYC { User: assetOwner })))
    (asserts! (is-some kyc-data) (err u100))
    (asserts! (get processCompleted (unwrap! kyc-data (err u101))) (err u102))
    
    (match (map-get? AptForApproval { assetOwner: assetOwner, assetTokeniztionRequest: assetNo})
      approval
        (ok approval)
      (err u200)
    )
  )
)
;; private functions
;;

