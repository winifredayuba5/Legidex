(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_STATUS (err u103))
(define-constant ERR_INVALID_ENTITY_TYPE (err u104))
(define-constant ERR_AGREEMENT_EXPIRED (err u105))
(define-constant ERR_NOT_SIGNATORY (err u106))
(define-constant ERR_DISPUTE_NOT_FOUND (err u107))
(define-constant ERR_INVALID_DISPUTE_STATUS (err u108))
(define-constant ERR_NOT_DISPUTE_PARTY (err u109))
(define-constant ERR_NOT_ARBITRATOR (err u110))
(define-constant ERR_ARBITRATOR_EXISTS (err u111))
(define-constant ERR_INSUFFICIENT_ESCROW (err u112))
(define-constant ERR_ESCROW_ALREADY_DEPOSITED (err u113))

(define-data-var next-entity-id uint u1)
(define-data-var next-agreement-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var registry-fee uint u1000000)
(define-data-var dispute-fee uint u500000)
(define-data-var arbitrator-fee uint u2000000)

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

(define-map disputes
  { dispute-id: uint }
  {
    agreement-id: uint,
    plaintiff: principal,
    defendant: principal,
    dispute-reason: (string-ascii 500),
    evidence-hash: (buff 32),
    status: (string-ascii 20),
    arbitrator: (optional principal),
    plaintiff-escrow: uint,
    defendant-escrow: uint,
    resolution: (string-ascii 500),
    winner: (optional principal),
    created-at: uint,
    resolved-at: (optional uint)
  }
)

(define-map dispute-votes
  { dispute-id: uint, voter: principal }
  {
    vote: (string-ascii 20),
    reasoning: (string-ascii 300),
    voted-at: uint
  }
)

(define-map certified-arbitrators
  { arbitrator: principal }
  {
    certification-level: uint,
    cases-resolved: uint,
    reputation-score: uint,
    specialization: (string-ascii 50),
    active: bool,
    certified-at: uint
  }
)

(define-map arbitrator-availability
  { arbitrator: principal, dispute-id: uint }
  {
    available: bool,
    proposed-fee: uint,
    acceptance-deadline: uint
  }
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

(define-public (register-arbitrator
  (specialization (string-ascii 50))
  (certification-level uint))
  (let
    (
      (current-block stacks-block-height)
    )
    (asserts! (is-none (map-get? certified-arbitrators { arbitrator: tx-sender })) ERR_ARBITRATOR_EXISTS)
    (asserts! (>= (stx-get-balance tx-sender) (var-get arbitrator-fee)) ERR_UNAUTHORIZED)
    (asserts! (<= certification-level u5) ERR_INVALID_STATUS)
    
    (try! (stx-transfer? (var-get arbitrator-fee) tx-sender CONTRACT_OWNER))
    
    (map-set certified-arbitrators
      { arbitrator: tx-sender }
      {
        certification-level: certification-level,
        cases-resolved: u0,
        reputation-score: u100,
        specialization: specialization,
        active: true,
        certified-at: current-block
      }
    )
    
    (ok true)
  )
)

(define-public (file-dispute
  (agreement-id uint)
  (defendant principal)
  (dispute-reason (string-ascii 500))
  (evidence-hash (buff 32)))
  (let
    (
      (dispute-id (var-get next-dispute-id))
      (current-block stacks-block-height)
      (agreement (unwrap! (map-get? legal-agreements { agreement-id: agreement-id }) ERR_NOT_FOUND))
    )
    (asserts! (>= (stx-get-balance tx-sender) (var-get dispute-fee)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status agreement) "ACTIVE") ERR_INVALID_STATUS)
    (asserts! (< current-block (get expiry-block agreement)) ERR_AGREEMENT_EXPIRED)
    
    (try! (stx-transfer? (var-get dispute-fee) tx-sender (as-contract tx-sender)))
    
    (map-set disputes
      { dispute-id: dispute-id }
      {
        agreement-id: agreement-id,
        plaintiff: tx-sender,
        defendant: defendant,
        dispute-reason: dispute-reason,
        evidence-hash: evidence-hash,
        status: "PENDING",
        arbitrator: none,
        plaintiff-escrow: u0,
        defendant-escrow: u0,
        resolution: "",
        winner: none,
        created-at: current-block,
        resolved-at: none
      }
    )
    
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

(define-public (deposit-dispute-escrow
  (dispute-id uint)
  (amount uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender (get plaintiff dispute)) 
                  (is-eq tx-sender (get defendant dispute))) ERR_NOT_DISPUTE_PARTY)
    (asserts! (is-eq (get status dispute) "PENDING") ERR_INVALID_DISPUTE_STATUS)
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR_INSUFFICIENT_ESCROW)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (if (is-eq tx-sender (get plaintiff dispute))
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute { plaintiff-escrow: (+ (get plaintiff-escrow dispute) amount) }))
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute { defendant-escrow: (+ (get defendant-escrow dispute) amount) }))
    )
    
    (ok true)
  )
)

(define-public (assign-arbitrator
  (dispute-id uint)
  (arbitrator principal))
  (let
    (
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (arbitrator-info (unwrap! (map-get? certified-arbitrators { arbitrator: arbitrator }) ERR_NOT_ARBITRATOR))
      (current-block stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender (get plaintiff dispute)) 
                  (is-eq tx-sender (get defendant dispute))) ERR_NOT_DISPUTE_PARTY)
    (asserts! (is-eq (get status dispute) "PENDING") ERR_INVALID_DISPUTE_STATUS)
    (asserts! (get active arbitrator-info) ERR_NOT_ARBITRATOR)
    
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute { 
        arbitrator: (some arbitrator),
        status: "IN_ARBITRATION"
      })
    )
    
    (ok true)
  )
)

(define-public (resolve-dispute
  (dispute-id uint)
  (winner principal)
  (resolution (string-ascii 500)))
  (let
    (
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (arbitrator (unwrap! (get arbitrator dispute) ERR_NOT_ARBITRATOR))
      (current-block stacks-block-height)
      (total-escrow (+ (get plaintiff-escrow dispute) (get defendant-escrow dispute)))
    )
    (asserts! (is-eq tx-sender arbitrator) ERR_NOT_ARBITRATOR)
    (asserts! (is-eq (get status dispute) "IN_ARBITRATION") ERR_INVALID_DISPUTE_STATUS)
    (asserts! (or (is-eq winner (get plaintiff dispute)) 
                  (is-eq winner (get defendant dispute))) ERR_NOT_DISPUTE_PARTY)
    
    (if (> total-escrow u0)
      (try! (as-contract (stx-transfer? total-escrow tx-sender winner)))
      true
    )
    
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute { 
        status: "RESOLVED",
        winner: (some winner),
        resolution: resolution,
        resolved-at: (some current-block)
      })
    )
    
    (match (map-get? certified-arbitrators { arbitrator: arbitrator })
      arbitrator-data 
        (map-set certified-arbitrators
          { arbitrator: arbitrator }
          (merge arbitrator-data {
            cases-resolved: (+ (get cases-resolved arbitrator-data) u1),
            reputation-score: (+ (get reputation-score arbitrator-data) u10)
          }))
      false
    )
    
    (ok true)
  )
)

(define-public (vote-on-dispute
  (dispute-id uint)
  (vote (string-ascii 20))
  (reasoning (string-ascii 300)))
  (let
    (
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (dao-member (unwrap! (map-get? dao-members { member: tx-sender }) ERR_NOT_SIGNATORY))
      (current-block stacks-block-height)
    )
    (asserts! (get is-active dao-member) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status dispute) "IN_ARBITRATION") ERR_INVALID_DISPUTE_STATUS)
    (asserts! (or (is-eq vote "PLAINTIFF") 
                  (is-eq vote "DEFENDANT") 
                  (is-eq vote "ABSTAIN")) ERR_INVALID_STATUS)
    
    (map-set dispute-votes
      { dispute-id: dispute-id, voter: tx-sender }
      {
        vote: vote,
        reasoning: reasoning,
        voted-at: current-block
      }
    )
    
    (ok true)
  )
)

(define-public (update-arbitrator-status
  (arbitrator principal)
  (active bool))
  (let
    (
      (arbitrator-info (unwrap! (map-get? certified-arbitrators { arbitrator: arbitrator }) ERR_NOT_ARBITRATOR))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set certified-arbitrators
      { arbitrator: arbitrator }
      (merge arbitrator-info { active: active })
    )
    
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

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-dispute-vote (dispute-id uint) (voter principal))
  (map-get? dispute-votes { dispute-id: dispute-id, voter: voter })
)

(define-read-only (get-arbitrator (arbitrator principal))
  (map-get? certified-arbitrators { arbitrator: arbitrator })
)

(define-read-only (get-arbitrator-availability (arbitrator principal) (dispute-id uint))
  (map-get? arbitrator-availability { arbitrator: arbitrator, dispute-id: dispute-id })
)

(define-read-only (get-next-dispute-id)
  (var-get next-dispute-id)
)

(define-read-only (get-dispute-fee)
  (var-get dispute-fee)
)

(define-read-only (get-arbitrator-fee)
  (var-get arbitrator-fee)
)

(define-read-only (is-certified-arbitrator (arbitrator principal))
  (match (map-get? certified-arbitrators { arbitrator: arbitrator })
    arbitrator-data (get active arbitrator-data)
    false
  )
)

(define-read-only (is-dispute-party (dispute-id uint) (user principal))
  (match (map-get? disputes { dispute-id: dispute-id })
    dispute (or 
              (is-eq (get plaintiff dispute) user)
              (is-eq (get defendant dispute) user))
    false
  )
)