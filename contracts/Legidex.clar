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
(define-constant ERR_AMENDMENT_NOT_FOUND (err u114))
(define-constant ERR_INVALID_AMENDMENT_STATUS (err u115))
(define-constant ERR_NOT_AMENDMENT_PARTY (err u116))
(define-constant ERR_ALREADY_APPROVED (err u117))
(define-constant ERR_VERSION_NOT_FOUND (err u118))
(define-constant ERR_INVALID_VERSION (err u119))

(define-data-var next-entity-id uint u1)
(define-data-var next-agreement-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var next-amendment-id uint u1)
(define-data-var registry-fee uint u1000000)
(define-data-var dispute-fee uint u500000)
(define-data-var arbitrator-fee uint u2000000)
(define-data-var amendment-fee uint u250000)

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

(define-map agreement-versions
  { agreement-id: uint, version: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    agreement-hash: (buff 32),
    changelog: (string-ascii 300),
    created-at: uint,
    created-by: principal,
    is-active: bool
  }
)

(define-map agreement-amendments
  { amendment-id: uint }
  {
    agreement-id: uint,
    current-version: uint,
    proposed-title: (string-ascii 100),
    proposed-description: (string-ascii 500),
    proposed-hash: (buff 32),
    amendment-reason: (string-ascii 300),
    changelog: (string-ascii 300),
    proposer: principal,
    status: (string-ascii 20),
    required-approvals: uint,
    current-approvals: uint,
    expiry-block: uint,
    created-at: uint,
    finalized-at: (optional uint)
  }
)

(define-map amendment-approvals
  { amendment-id: uint, approver: principal }
  {
    approved: bool,
    approval-signature: (buff 32),
    approved-at: uint,
    notes: (string-ascii 200)
  }
)

(define-map agreement-signatories-count
  { agreement-id: uint }
  { count: uint }
)

(define-map version-transition-log
  { agreement-id: uint, from-version: uint, to-version: uint }
  {
    amendment-id: uint,
    transition-type: (string-ascii 20),
    transition-date: uint,
    authorized-by: principal
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
    
    (map-set agreement-versions
      { agreement-id: agreement-id, version: u1 }
      {
        title: title,
        description: description,
        agreement-hash: agreement-hash,
        changelog: "Initial version",
        created-at: current-block,
        created-by: tx-sender,
        is-active: true
      }
    )
    
    (map-set agreement-signatories-count
      { agreement-id: agreement-id }
      { count: u0 }
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
    
    (match (map-get? agreement-signatories-count { agreement-id: agreement-id })
      current-count 
        (map-set agreement-signatories-count
          { agreement-id: agreement-id }
          { count: (+ (get count current-count) u1) })
      (map-set agreement-signatories-count
        { agreement-id: agreement-id }
        { count: u1 })
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

(define-public (propose-amendment
  (agreement-id uint)
  (proposed-title (string-ascii 100))
  (proposed-description (string-ascii 500))
  (proposed-hash (buff 32))
  (amendment-reason (string-ascii 300))
  (changelog (string-ascii 300))
  (expiry-blocks uint))
  (let
    (
      (amendment-id (var-get next-amendment-id))
      (current-block stacks-block-height)
      (expiry-block (+ current-block expiry-blocks))
      (agreement (unwrap! (map-get? legal-agreements { agreement-id: agreement-id }) ERR_NOT_FOUND))
      (signatories-count (unwrap! (map-get? agreement-signatories-count { agreement-id: agreement-id }) ERR_NOT_FOUND))
      (current-version (get-current-version agreement-id))
    )
    (asserts! (>= (stx-get-balance tx-sender) (var-get amendment-fee)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status agreement) "ACTIVE") ERR_INVALID_STATUS)
    (asserts! (< current-block (get expiry-block agreement)) ERR_AGREEMENT_EXPIRED)
    (asserts! (or (is-eq tx-sender (get creator agreement))
                  (is-some (map-get? agreement-signatories { agreement-id: agreement-id, signatory: tx-sender }))) ERR_NOT_SIGNATORY)
    
    (try! (stx-transfer? (var-get amendment-fee) tx-sender (as-contract tx-sender)))
    
    (map-set agreement-amendments
      { amendment-id: amendment-id }
      {
        agreement-id: agreement-id,
        current-version: current-version,
        proposed-title: proposed-title,
        proposed-description: proposed-description,
        proposed-hash: proposed-hash,
        amendment-reason: amendment-reason,
        changelog: changelog,
        proposer: tx-sender,
        status: "PENDING",
        required-approvals: (get count signatories-count),
        current-approvals: u0,
        expiry-block: expiry-block,
        created-at: current-block,
        finalized-at: none
      }
    )
    
    (var-set next-amendment-id (+ amendment-id u1))
    (ok amendment-id)
  )
)

(define-public (approve-amendment
  (amendment-id uint)
  (approval-signature (buff 32))
  (notes (string-ascii 200)))
  (let
    (
      (amendment (unwrap! (map-get? agreement-amendments { amendment-id: amendment-id }) ERR_AMENDMENT_NOT_FOUND))
      (current-block stacks-block-height)
      (agreement-id (get agreement-id amendment))
      (existing-approval (map-get? amendment-approvals { amendment-id: amendment-id, approver: tx-sender }))
    )
    (asserts! (is-eq (get status amendment) "PENDING") ERR_INVALID_AMENDMENT_STATUS)
    (asserts! (< current-block (get expiry-block amendment)) ERR_AGREEMENT_EXPIRED)
    (asserts! (is-some (map-get? agreement-signatories { agreement-id: agreement-id, signatory: tx-sender })) ERR_NOT_SIGNATORY)
    (asserts! (is-none existing-approval) ERR_ALREADY_APPROVED)
    
    (map-set amendment-approvals
      { amendment-id: amendment-id, approver: tx-sender }
      {
        approved: true,
        approval-signature: approval-signature,
        approved-at: current-block,
        notes: notes
      }
    )
    
    (map-set agreement-amendments
      { amendment-id: amendment-id }
      (merge amendment { current-approvals: (+ (get current-approvals amendment) u1) })
    )
    
    (ok true)
  )
)

(define-public (finalize-amendment (amendment-id uint))
  (let
    (
      (amendment (unwrap! (map-get? agreement-amendments { amendment-id: amendment-id }) ERR_AMENDMENT_NOT_FOUND))
      (current-block stacks-block-height)
      (agreement-id (get agreement-id amendment))
      (agreement (unwrap! (map-get? legal-agreements { agreement-id: agreement-id }) ERR_NOT_FOUND))
      (current-version (get-current-version agreement-id))
      (new-version (+ current-version u1))
    )
    (asserts! (is-eq (get status amendment) "PENDING") ERR_INVALID_AMENDMENT_STATUS)
    (asserts! (>= (get current-approvals amendment) (get required-approvals amendment)) ERR_UNAUTHORIZED)
    (asserts! (< current-block (get expiry-block amendment)) ERR_AGREEMENT_EXPIRED)
    (asserts! (is-eq tx-sender (get proposer amendment)) ERR_UNAUTHORIZED)
    
    (map-set agreement-versions
      { agreement-id: agreement-id, version: current-version }
      (merge (unwrap! (map-get? agreement-versions { agreement-id: agreement-id, version: current-version }) ERR_VERSION_NOT_FOUND) 
             { is-active: false })
    )
    
    (map-set agreement-versions
      { agreement-id: agreement-id, version: new-version }
      {
        title: (get proposed-title amendment),
        description: (get proposed-description amendment),
        agreement-hash: (get proposed-hash amendment),
        changelog: (get changelog amendment),
        created-at: current-block,
        created-by: tx-sender,
        is-active: true
      }
    )
    
    (map-set legal-agreements
      { agreement-id: agreement-id }
      (merge agreement {
        title: (get proposed-title amendment),
        description: (get proposed-description amendment),
        agreement-hash: (get proposed-hash amendment),
        updated-at: current-block
      })
    )
    
    (map-set version-transition-log
      { agreement-id: agreement-id, from-version: current-version, to-version: new-version }
      {
        amendment-id: amendment-id,
        transition-type: "AMENDMENT",
        transition-date: current-block,
        authorized-by: tx-sender
      }
    )
    
    (map-set agreement-amendments
      { amendment-id: amendment-id }
      (merge amendment { 
        status: "APPROVED",
        finalized-at: (some current-block)
      })
    )
    
    (ok new-version)
  )
)

(define-public (reject-amendment
  (amendment-id uint)
  (rejection-reason (string-ascii 200)))
  (let
    (
      (amendment (unwrap! (map-get? agreement-amendments { amendment-id: amendment-id }) ERR_AMENDMENT_NOT_FOUND))
      (current-block stacks-block-height)
      (agreement-id (get agreement-id amendment))
    )
    (asserts! (is-eq (get status amendment) "PENDING") ERR_INVALID_AMENDMENT_STATUS)
    (asserts! (is-some (map-get? agreement-signatories { agreement-id: agreement-id, signatory: tx-sender })) ERR_NOT_SIGNATORY)
    
    (map-set amendment-approvals
      { amendment-id: amendment-id, approver: tx-sender }
      {
        approved: false,
        approval-signature: 0x00,
        approved-at: current-block,
        notes: rejection-reason
      }
    )
    
    (map-set agreement-amendments
      { amendment-id: amendment-id }
      (merge amendment { 
        status: "REJECTED",
        finalized-at: (some current-block)
      })
    )
    
    (ok true)
  )
)

(define-public (withdraw-amendment (amendment-id uint))
  (let
    (
      (amendment (unwrap! (map-get? agreement-amendments { amendment-id: amendment-id }) ERR_AMENDMENT_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get proposer amendment)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status amendment) "PENDING") ERR_INVALID_AMENDMENT_STATUS)
    
    (map-set agreement-amendments
      { amendment-id: amendment-id }
      (merge amendment { 
        status: "WITHDRAWN",
        finalized-at: (some current-block)
      })
    )
    
    (ok true)
  )
)

(define-read-only (get-current-version (agreement-id uint))
  (get max-version
    (fold find-max-version 
          (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20)
          { agreement-id: agreement-id, max-version: u1 })))

(define-private (find-max-version (version uint) (acc { agreement-id: uint, max-version: uint }))
  (if (is-some (map-get? agreement-versions { agreement-id: (get agreement-id acc), version: version }))
    { agreement-id: (get agreement-id acc), max-version: version }
    acc))

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

(define-read-only (get-agreement-version (agreement-id uint) (version uint))
  (map-get? agreement-versions { agreement-id: agreement-id, version: version })
)

(define-read-only (get-current-agreement-version (agreement-id uint))
  (let
    (
      (current-version (get-current-version agreement-id))
    )
    (map-get? agreement-versions { agreement-id: agreement-id, version: current-version })
  )
)

(define-read-only (get-amendment (amendment-id uint))
  (map-get? agreement-amendments { amendment-id: amendment-id })
)

(define-read-only (get-amendment-approval (amendment-id uint) (approver principal))
  (map-get? amendment-approvals { amendment-id: amendment-id, approver: approver })
)

(define-read-only (get-version-transition (agreement-id uint) (from-version uint) (to-version uint))
  (map-get? version-transition-log { agreement-id: agreement-id, from-version: from-version, to-version: to-version })
)

(define-read-only (get-agreement-signatories-count (agreement-id uint))
  (map-get? agreement-signatories-count { agreement-id: agreement-id })
)

(define-read-only (get-next-amendment-id)
  (var-get next-amendment-id)
)

(define-read-only (get-amendment-fee)
  (var-get amendment-fee)
)

(define-read-only (is-amendment-approver (amendment-id uint) (user principal))
  (match (map-get? amendment-approvals { amendment-id: amendment-id, approver: user })
    approval (get approved approval)
    false
  )
)

(define-read-only (get-agreement-version-history (agreement-id uint))
  (list 
    (map-get? agreement-versions { agreement-id: agreement-id, version: u1 })
    (map-get? agreement-versions { agreement-id: agreement-id, version: u2 })
    (map-get? agreement-versions { agreement-id: agreement-id, version: u3 })
    (map-get? agreement-versions { agreement-id: agreement-id, version: u4 })
    (map-get? agreement-versions { agreement-id: agreement-id, version: u5 })
    (map-get? agreement-versions { agreement-id: agreement-id, version: u6 })
    (map-get? agreement-versions { agreement-id: agreement-id, version: u7 })
    (map-get? agreement-versions { agreement-id: agreement-id, version: u8 })
    (map-get? agreement-versions { agreement-id: agreement-id, version: u9 })
    (map-get? agreement-versions { agreement-id: agreement-id, version: u10 })
  )
)

