(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_STATUS (err u103))
(define-constant ERR_INVALID_ENTITY_TYPE (err u104))
(define-constant ERR_AGREEMENT_EXPIRED (err u105))
(define-constant ERR_NOT_SIGNATORY (err u106))

(define-data-var next-entity-id uint u1)
(define-data-var next-agreement-id uint u1)
(define-data-var registry-fee uint u1000000)

(define-map legal-entities
  { entity-id: uint }
  {
    name: (string-ascii 100),
    entity-type: (string-ascii 20),
    jurisdiction: (string-ascii 50),
    registration-number: (string-ascii 50),
    owner: principal,
    status: (string-ascii 20),
    created-at: uint,
    updated-at: uint
  }
)

(define-map legal-agreements
  { agreement-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    agreement-hash: (buff 32),
    creator: principal,
    status: (string-ascii 20),
    expiry-block: uint,
    created-at: uint,
    updated-at: uint
  }
)

(define-map entity-agreements
  { entity-id: uint, agreement-id: uint }
  { signed-at: uint, signature-hash: (buff 32) }
)

(define-map agreement-signatories
  { agreement-id: uint, signatory: principal }
  { signed-at: uint, signature-hash: (buff 32) }
)

(define-map dao-members
  { member: principal }
  { 
    joined-at: uint,
    reputation: uint,
    is-active: bool
  }
)

(define-map entity-owners
  { entity-id: uint }
  { owner: principal }
)

(define-public (register-legal-entity 
  (name (string-ascii 100))
  (entity-type (string-ascii 20))
  (jurisdiction (string-ascii 50))
  (registration-number (string-ascii 50)))
  (let
    (
      (entity-id (var-get next-entity-id))
      (current-block stacks-block-height)
    )
    (asserts! (>= (stx-get-balance tx-sender) (var-get registry-fee)) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq entity-type "LLC") 
                  (is-eq entity-type "CORP") 
                  (is-eq entity-type "DAO") 
                  (is-eq entity-type "TRUST")) ERR_INVALID_ENTITY_TYPE)
    
    (try! (stx-transfer? (var-get registry-fee) tx-sender CONTRACT_OWNER))
    
    (map-set legal-entities
      { entity-id: entity-id }
      {
        name: name,
        entity-type: entity-type,
        jurisdiction: jurisdiction,
        registration-number: registration-number,
        owner: tx-sender,
        status: "ACTIVE",
        created-at: current-block,
        updated-at: current-block
      }
    )
    
    (map-set entity-owners
      { entity-id: entity-id }
      { owner: tx-sender }
    )
    
    (var-set next-entity-id (+ entity-id u1))
    (ok entity-id)
  )
)

(define-public (create-legal-agreement
  (title (string-ascii 100))
  (description (string-ascii 500))
  (agreement-hash (buff 32))
  (expiry-blocks uint))
  (let
    (
      (agreement-id (var-get next-agreement-id))
      (current-block stacks-block-height)
      (expiry-block (+ current-block expiry-blocks))
    )
    (map-set legal-agreements
      { agreement-id: agreement-id }
      {
        title: title,
        description: description,
        agreement-hash: agreement-hash,
        creator: tx-sender,
        status: "DRAFT",
        expiry-block: expiry-block,
        created-at: current-block,
        updated-at: current-block
      }
    )
    
    (var-set next-agreement-id (+ agreement-id u1))
    (ok agreement-id)
  )
)

(define-public (sign-agreement
  (agreement-id uint)
  (signature-hash (buff 32)))
  (let
    (
      (agreement (unwrap! (map-get? legal-agreements { agreement-id: agreement-id }) ERR_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (< current-block (get expiry-block agreement)) ERR_AGREEMENT_EXPIRED)
    (asserts! (is-eq (get status agreement) "ACTIVE") ERR_INVALID_STATUS)
    
    (map-set agreement-signatories
      { agreement-id: agreement-id, signatory: tx-sender }
      { signed-at: current-block, signature-hash: signature-hash }
    )
    
    (ok true)
  )
)

(define-public (activate-agreement (agreement-id uint))
  (let
    (
      (agreement (unwrap! (map-get? legal-agreements { agreement-id: agreement-id }) ERR_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get creator agreement)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status agreement) "DRAFT") ERR_INVALID_STATUS)
    
    (map-set legal-agreements
      { agreement-id: agreement-id }
      (merge agreement { status: "ACTIVE", updated-at: current-block })
    )
    
    (ok true)
  )
)

(define-public (link-entity-agreement
  (entity-id uint)
  (agreement-id uint)
  (signature-hash (buff 32)))
  (let
    (
      (entity (unwrap! (map-get? legal-entities { entity-id: entity-id }) ERR_NOT_FOUND))
      (agreement (unwrap! (map-get? legal-agreements { agreement-id: agreement-id }) ERR_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get owner entity)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status agreement) "ACTIVE") ERR_INVALID_STATUS)
    (asserts! (< current-block (get expiry-block agreement)) ERR_AGREEMENT_EXPIRED)
    
    (map-set entity-agreements
      { entity-id: entity-id, agreement-id: agreement-id }
      { signed-at: current-block, signature-hash: signature-hash }
    )
    
    (ok true)
  )
)

(define-public (update-entity-status
  (entity-id uint)
  (new-status (string-ascii 20)))
  (let
    (
      (entity (unwrap! (map-get? legal-entities { entity-id: entity-id }) ERR_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get owner entity)) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq new-status "ACTIVE") 
                  (is-eq new-status "SUSPENDED") 
                  (is-eq new-status "DISSOLVED")) ERR_INVALID_STATUS)
    
    (map-set legal-entities
      { entity-id: entity-id }
      (merge entity { status: new-status, updated-at: current-block })
    )
    
    (ok true)
  )
)

(define-public (join-dao)
  (let
    (
      (current-block stacks-block-height)
    )
    (asserts! (is-none (map-get? dao-members { member: tx-sender })) ERR_ALREADY_EXISTS)
    
    (map-set dao-members
      { member: tx-sender }
      {
        joined-at: current-block,
        reputation: u0,
        is-active: true
      }
    )
    
    (ok true)
  )
)

(define-public (update-registry-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set registry-fee new-fee)
    (ok true)
  )
)

(define-read-only (get-legal-entity (entity-id uint))
  (map-get? legal-entities { entity-id: entity-id })
)

(define-read-only (get-legal-agreement (agreement-id uint))
  (map-get? legal-agreements { agreement-id: agreement-id })
)

(define-read-only (get-entity-agreement (entity-id uint) (agreement-id uint))
  (map-get? entity-agreements { entity-id: entity-id, agreement-id: agreement-id })
)

(define-read-only (get-agreement-signature (agreement-id uint) (signatory principal))
  (map-get? agreement-signatories { agreement-id: agreement-id, signatory: signatory })
)

(define-read-only (get-dao-member (member principal))
  (map-get? dao-members { member: member })
)

(define-read-only (get-next-entity-id)
  (var-get next-entity-id)
)

(define-read-only (get-next-agreement-id)
  (var-get next-agreement-id)
)

(define-read-only (get-registry-fee)
  (var-get registry-fee)
)

(define-read-only (is-entity-owner (entity-id uint) (user principal))
  (match (map-get? legal-entities { entity-id: entity-id })
    entity (is-eq (get owner entity) user)
    false
  )
)

(define-read-only (is-agreement-active (agreement-id uint))
  (match (map-get? legal-agreements { agreement-id: agreement-id })
    agreement (and 
                (is-eq (get status agreement) "ACTIVE")
                (< stacks-block-height (get expiry-block agreement)))
    false
  )
)