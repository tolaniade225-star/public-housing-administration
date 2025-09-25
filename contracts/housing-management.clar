;; Public Housing Administration Smart Contract
;; Manages affordable housing applications, tenant qualifications, rent calculations, and maintenance coordination

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ALREADY_EXISTS (err u101))
(define-constant ERR_NOT_FOUND (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))

;; Data Variables
(define-data-var next-tenant-id uint u1)
(define-data-var next-unit-id uint u1)
(define-data-var next-maintenance-id uint u1)

;; Data Maps
(define-map tenants uint {
  address: principal,
  name: (string-ascii 50),
  income: uint,
  family-size: uint,
  qualified: bool,
  unit-id: (optional uint),
  rent-amount: uint,
  payment-due: uint
})

(define-map housing-units uint {
  address: (string-ascii 100),
  bedrooms: uint,
  rent-base: uint,
  occupied: bool,
  tenant-id: (optional uint),
  last-inspection: uint
})

(define-map maintenance-requests uint {
  unit-id: uint,
  tenant-id: uint,
  description: (string-ascii 200),
  priority: (string-ascii 20),
  status: (string-ascii 20),
  created-at: uint,
  completed-at: (optional uint)
})

(define-map rent-payments {
  tenant-id: uint,
  period: uint
} {
  amount: uint,
  paid-at: uint,
  late-fee: uint
})

;; Private Functions
(define-private (calculate-adjusted-rent (base-rent uint) (income uint) (family-size uint))
  (let (
    (income-ratio (/ (* income u100) u50000))
    (family-discount (if (>= family-size u4) u15 u0))
    (adjusted-amount (- base-rent (/ (* base-rent (+ income-ratio family-discount)) u100)))
  )
  (if (< adjusted-amount (/ base-rent u4))
    (/ base-rent u4)
    adjusted-amount
  ))
)

(define-private (is-qualified (income uint) (family-size uint))
  (and 
    (<= income u60000)
    (>= family-size u1)
  )
)

;; Public Functions
(define-public (register-tenant (name (string-ascii 50)) (income uint) (family-size uint))
  (let (
    (tenant-id (var-get next-tenant-id))
    (qualified (is-qualified income family-size))
  )
  (map-set tenants tenant-id {
    address: tx-sender,
    name: name,
    income: income,
    family-size: family-size,
    qualified: qualified,
    unit-id: none,
    rent-amount: u0,
    payment-due: u0
  })
  (var-set next-tenant-id (+ tenant-id u1))
  (ok tenant-id)
  )
)

(define-public (add-housing-unit (address (string-ascii 100)) (bedrooms uint) (rent-base uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (let ((unit-id (var-get next-unit-id)))
      (map-set housing-units unit-id {
        address: address,
        bedrooms: bedrooms,
        rent-base: rent-base,
        occupied: false,
        tenant-id: none,
        last-inspection: u0
      })
      (var-set next-unit-id (+ unit-id u1))
      (ok unit-id)
    )
  )
)

(define-public (assign-unit (tenant-id uint) (unit-id uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (let (
      (tenant (unwrap! (map-get? tenants tenant-id) ERR_NOT_FOUND))
      (unit (unwrap! (map-get? housing-units unit-id) ERR_NOT_FOUND))
    )
    (asserts! (get qualified tenant) ERR_NOT_AUTHORIZED)
    (asserts! (not (get occupied unit)) ERR_ALREADY_EXISTS)
    (let (
      (adjusted-rent (calculate-adjusted-rent (get rent-base unit) (get income tenant) (get family-size tenant)))
    )
    (map-set tenants tenant-id (merge tenant {
      unit-id: (some unit-id),
      rent-amount: adjusted-rent,
      payment-due: u720
    }))
    (map-set housing-units unit-id (merge unit {
      occupied: true,
      tenant-id: (some tenant-id)
    }))
    (ok true)
    )
    )
  )
)

(define-public (record-rent-payment (tenant-id uint) (amount uint))
  (let (
    (tenant (unwrap! (map-get? tenants tenant-id) ERR_NOT_FOUND))
    (payment-period u1)
  )
  (asserts! (> amount u0) ERR_INVALID_AMOUNT)
  (map-set rent-payments { tenant-id: tenant-id, period: payment-period } {
    amount: amount,
    paid-at: u1,
    late-fee: u0
  })
  (map-set tenants tenant-id (merge tenant {
    payment-due: u720
  }))
  (ok true)
  )
)

(define-public (submit-maintenance-request (unit-id uint) (description (string-ascii 200)) (priority (string-ascii 20)))
  (let (
    (unit (unwrap! (map-get? housing-units unit-id) ERR_NOT_FOUND))
    (tenant-id (unwrap! (get tenant-id unit) ERR_NOT_FOUND))
    (request-id (var-get next-maintenance-id))
  )
  (asserts! (is-eq tx-sender (get address (unwrap! (map-get? tenants tenant-id) ERR_NOT_FOUND))) ERR_NOT_AUTHORIZED)
  (map-set maintenance-requests request-id {
    unit-id: unit-id,
    tenant-id: tenant-id,
    description: description,
    priority: priority,
    status: "pending",
    created-at: u1,
    completed-at: none
  })
  (var-set next-maintenance-id (+ request-id u1))
  (ok request-id)
  )
)

(define-public (complete-maintenance (request-id uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (let ((request (unwrap! (map-get? maintenance-requests request-id) ERR_NOT_FOUND)))
      (map-set maintenance-requests request-id (merge request {
        status: "completed",
        completed-at: (some u1)
      }))
      (ok true)
    )
  )
)

;; Read-Only Functions
(define-read-only (get-tenant (tenant-id uint))
  (map-get? tenants tenant-id)
)

(define-read-only (get-housing-unit (unit-id uint))
  (map-get? housing-units unit-id)
)

(define-read-only (get-maintenance-request (request-id uint))
  (map-get? maintenance-requests request-id)
)

(define-read-only (get-rent-payment (tenant-id uint) (period uint))
  (map-get? rent-payments { tenant-id: tenant-id, period: period })
)
