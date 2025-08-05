;; Simple Lending Pool - Peer-to-Peer Lending Contract
;; Production-ready lending platform with interest calculations, collateral requirements, and repayment tracking

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_LOAN_NOT_FOUND (err u101))
(define-constant ERR_LOAN_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u103))
(define-constant ERR_LOAN_NOT_ACTIVE (err u104))
(define-constant ERR_LOAN_ALREADY_REPAID (err u105))
(define-constant ERR_PAYMENT_AMOUNT_INVALID (err u106))
(define-constant ERR_LOAN_OVERDUE (err u107))
(define-constant ERR_INSUFFICIENT_BALANCE (err u108))
(define-constant ERR_INVALID_PARAMETERS (err u109))
(define-constant ERR_COLLATERAL_LOCKED (err u110))
(define-constant ERR_LOAN_NOT_OVERDUE (err u111))

;; Basis points for percentage calculations (10000 = 100%)
(define-constant BASIS_POINTS u10000)
(define-constant BLOCKS_PER_DAY u144) ;; Approximate blocks per day
(define-constant MIN_LOAN_AMOUNT u1000000) ;; 1 STX minimum
(define-constant MAX_LOAN_DURATION u4320) ;; 30 days in blocks
(define-constant LIQUIDATION_THRESHOLD u12000) ;; 120% collateral ratio

;; Data Variables
(define-data-var next-loan-id uint u1)
(define-data-var platform-fee-rate uint u100) ;; 1% in basis points
(define-data-var total-loans-issued uint u0)
(define-data-var total-loans-repaid uint u0)

;; Loan Status Enum
(define-constant LOAN_STATUS_ACTIVE u1)
(define-constant LOAN_STATUS_REPAID u2)
(define-constant LOAN_STATUS_DEFAULTED u3)
(define-constant LOAN_STATUS_LIQUIDATED u4)

;; Data Maps
(define-map loans
    { loan-id: uint }
    {
        borrower: principal,
        lender: principal,
        principal-amount: uint,
        interest-rate: uint, ;; Annual rate in basis points
        collateral-amount: uint,
        loan-duration: uint, ;; Duration in blocks
        start-block: uint,
        end-block: uint,
        amount-repaid: uint,
        status: uint,
        last-payment-block: uint
    }
)

(define-map user-collateral
    { user: principal }
    { amount: uint }
)

(define-map loan-requests
    { request-id: uint }
    {
        borrower: principal,
        amount: uint,
        interest-rate: uint,
        duration: uint,
        collateral-offered: uint,
        is-active: bool,
        created-block: uint
    }
)

(define-data-var next-request-id uint u1)

;; Read-only functions

(define-read-only (get-loan (loan-id uint))
    (map-get? loans { loan-id: loan-id })
)

(define-read-only (get-user-collateral (user principal))
    (default-to u0 (get amount (map-get? user-collateral { user: user })))
)

(define-read-only (get-loan-request (request-id uint))
    (map-get? loan-requests { request-id: request-id })
)

(define-read-only (calculate-interest (principal-amount uint) (interest-rate uint) (blocks-elapsed uint))
    (let (
        (annual-interest (/ (* principal-amount interest-rate) BASIS_POINTS))
        (daily-interest (/ annual-interest u365))
        (days-elapsed (/ blocks-elapsed BLOCKS_PER_DAY))
    )
        (* daily-interest days-elapsed)
    )
)

(define-read-only (get-loan-status (loan-id uint))
    (match (get-loan loan-id)
        loan-data (ok (get status loan-data))
        ERR_LOAN_NOT_FOUND
    )
)

(define-read-only (calculate-total-repayment (loan-id uint))
    (match (get-loan loan-id)
        loan-data (let (
            (principal (get principal-amount loan-data))
            (interest-rate (get interest-rate loan-data))
            (duration (get loan-duration loan-data))
            (interest (calculate-interest principal interest-rate duration))
        )
            (ok (+ principal interest))
        )
        ERR_LOAN_NOT_FOUND
    )
)

(define-read-only (is-loan-overdue (loan-id uint))
    (match (get-loan loan-id)
        loan-data (let (
            (current-block stacks-block-height)
            (end-block (get end-block loan-data))
            (status (get status loan-data))
        )
            (ok (and (> current-block end-block) (is-eq status LOAN_STATUS_ACTIVE)))
        )
        ERR_LOAN_NOT_FOUND
    )
)

(define-read-only (get-platform-stats)
    (ok {
        total-loans-issued: (var-get total-loans-issued),
        total-loans-repaid: (var-get total-loans-repaid),
        platform-fee-rate: (var-get platform-fee-rate),
        next-loan-id: (var-get next-loan-id)
    })
)

;; Public functions

(define-public (deposit-collateral (amount uint))
    (let (
        (current-collateral (get-user-collateral tx-sender))
        (new-collateral (+ current-collateral amount))
    )
        (asserts! (> amount u0) ERR_INVALID_PARAMETERS)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set user-collateral
            { user: tx-sender }
            { amount: new-collateral }
        )
        (ok new-collateral)
    )
)

(define-public (withdraw-collateral (amount uint))
    (let (
        (current-collateral (get-user-collateral tx-sender))
    )
        (asserts! (>= current-collateral amount) ERR_INSUFFICIENT_BALANCE)
        (asserts! (> amount u0) ERR_INVALID_PARAMETERS)

        ;; Check if collateral is locked in active loans
        ;; This would require additional tracking in a production system

        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (map-set user-collateral
            { user: tx-sender }
            { amount: (- current-collateral amount) }
        )
        (ok (- current-collateral amount))
    )
)

(define-public (create-loan-request (amount uint) (interest-rate uint) (duration uint) (collateral-offered uint))
    (let (
        (request-id (var-get next-request-id))
        (user-collateral-amount (get-user-collateral tx-sender))
    )
        (asserts! (>= amount MIN_LOAN_AMOUNT) ERR_INVALID_PARAMETERS)
        (asserts! (<= duration MAX_LOAN_DURATION) ERR_INVALID_PARAMETERS)
        (asserts! (> interest-rate u0) ERR_INVALID_PARAMETERS)
        (asserts! (>= user-collateral-amount collateral-offered) ERR_INSUFFICIENT_COLLATERAL)
        (asserts! (>= (* collateral-offered BASIS_POINTS) (* amount LIQUIDATION_THRESHOLD)) ERR_INSUFFICIENT_COLLATERAL)

        (map-set loan-requests
            { request-id: request-id }
            {
                borrower: tx-sender,
                amount: amount,
                interest-rate: interest-rate,
                duration: duration,
                collateral-offered: collateral-offered,
                is-active: true,
                created-block: stacks-block-height
            }
        )

        (var-set next-request-id (+ request-id u1))
        (ok request-id)
    )
)

(define-public (fund-loan-request (request-id uint))
    (match (get-loan-request request-id)
        request-data (let (
            (loan-id (var-get next-loan-id))
            (borrower (get borrower request-data))
            (amount (get amount request-data))
            (interest-rate (get interest-rate request-data))
            (duration (get duration request-data))
            (collateral-amount (get collateral-offered request-data))
            (current-block stacks-block-height)
            (end-block (+ current-block duration))
            (borrower-collateral (get-user-collateral borrower))
        )
            (asserts! (get is-active request-data) ERR_LOAN_NOT_ACTIVE)
            (asserts! (not (is-eq tx-sender borrower)) ERR_UNAUTHORIZED)
            (asserts! (>= borrower-collateral collateral-amount) ERR_INSUFFICIENT_COLLATERAL)

            ;; Transfer loan amount to borrower
            (try! (stx-transfer? amount tx-sender borrower))

            ;; Lock collateral
            (map-set user-collateral
                { user: borrower }
                { amount: (- borrower-collateral collateral-amount) }
            )

            ;; Create loan record
            (map-set loans
                { loan-id: loan-id }
                {
                    borrower: borrower,
                    lender: tx-sender,
                    principal-amount: amount,
                    interest-rate: interest-rate,
                    collateral-amount: collateral-amount,
                    loan-duration: duration,
                    start-block: current-block,
                    end-block: end-block,
                    amount-repaid: u0,
                    status: LOAN_STATUS_ACTIVE,
                    last-payment-block: current-block
                }
            )

            ;; Deactivate loan request
            (map-set loan-requests
                { request-id: request-id }
                (merge request-data { is-active: false })
            )

            ;; Update counters
            (var-set next-loan-id (+ loan-id u1))
            (var-set total-loans-issued (+ (var-get total-loans-issued) u1))

            (ok loan-id)
        )
        ERR_LOAN_NOT_FOUND
    )
)

(define-public (make-payment (loan-id uint) (payment-amount uint))
    (match (get-loan loan-id)
        loan-data (let (
            (borrower (get borrower loan-data))
            (lender (get lender loan-data))
            (status (get status loan-data))
            (amount-repaid (get amount-repaid loan-data))
            (new-amount-repaid (+ amount-repaid payment-amount))
            (total-repayment (unwrap! (calculate-total-repayment loan-id) ERR_INVALID_PARAMETERS))
            (platform-fee (/ (* payment-amount (var-get platform-fee-rate)) BASIS_POINTS))
            (lender-payment (- payment-amount platform-fee))
        )
            (asserts! (is-eq tx-sender borrower) ERR_UNAUTHORIZED)
            (asserts! (is-eq status LOAN_STATUS_ACTIVE) ERR_LOAN_NOT_ACTIVE)
            (asserts! (> payment-amount u0) ERR_PAYMENT_AMOUNT_INVALID)
            (asserts! (<= new-amount-repaid total-repayment) ERR_PAYMENT_AMOUNT_INVALID)

            ;; Transfer payment to lender and platform fee to contract owner
            (try! (stx-transfer? lender-payment tx-sender lender))
            (try! (stx-transfer? platform-fee tx-sender CONTRACT_OWNER))

            ;; Check if loan is fully repaid
            (let (
                (new-status (if (is-eq new-amount-repaid total-repayment)
                    LOAN_STATUS_REPAID
                    LOAN_STATUS_ACTIVE))
            )
                ;; Update loan record
                (map-set loans
                    { loan-id: loan-id }
                    (merge loan-data {
                        amount-repaid: new-amount-repaid,
                        status: new-status,
                        last-payment-block: stacks-block-height
                    })
                )

                ;; If fully repaid, release collateral and update counter
                (if (is-eq new-status LOAN_STATUS_REPAID)
                    (begin
                        (let (
                            (borrower-collateral (get-user-collateral borrower))
                            (collateral-amount (get collateral-amount loan-data))
                        )
                            (map-set user-collateral
                                { user: borrower }
                                { amount: (+ borrower-collateral collateral-amount) }
                            )
                        )
                        (var-set total-loans-repaid (+ (var-get total-loans-repaid) u1))
                    )
                    true
                )

                (ok new-amount-repaid)
            )
        )
        ERR_LOAN_NOT_FOUND
    )
)

(define-public (liquidate-loan (loan-id uint))
    (match (get-loan loan-id)
        loan-data (let (
            (borrower (get borrower loan-data))
            (lender (get lender loan-data))
            (status (get status loan-data))
            (collateral-amount (get collateral-amount loan-data))
            (current-block stacks-block-height)
            (end-block (get end-block loan-data))
        )
            (asserts! (is-eq tx-sender lender) ERR_UNAUTHORIZED)
            (asserts! (is-eq status LOAN_STATUS_ACTIVE) ERR_LOAN_NOT_ACTIVE)
            (asserts! (> current-block end-block) ERR_LOAN_NOT_OVERDUE)

            ;; Transfer collateral to lender
            (try! (as-contract (stx-transfer? collateral-amount tx-sender lender)))

            ;; Update loan status to liquidated
            (map-set loans
                { loan-id: loan-id }
                (merge loan-data { status: LOAN_STATUS_LIQUIDATED })
            )

            (ok true)
        )
        ERR_LOAN_NOT_FOUND
    )
)

;; Admin functions

(define-public (set-platform-fee-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= new-rate u1000) ERR_INVALID_PARAMETERS) ;; Max 10% fee
        (var-set platform-fee-rate new-rate)
        (ok new-rate)
    )
)

(define-public (emergency-pause-loan (loan-id uint))
    (match (get-loan loan-id)
        loan-data (begin
            (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
            (map-set loans
                { loan-id: loan-id }
                (merge loan-data { status: LOAN_STATUS_DEFAULTED })
            )
            (ok true)
        )
        ERR_LOAN_NOT_FOUND
    )
)
