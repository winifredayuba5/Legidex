;; Entity Compliance Manager
;; Tracks regulatory requirements, certifications, and compliance deadlines for legal entities

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_NOT_FOUND (err u201))
(define-constant ERR_ALREADY_EXISTS (err u202))
(define-constant ERR_INVALID_STATUS (err u203))
(define-constant ERR_INVALID_PRIORITY (err u204))
(define-constant ERR_DEADLINE_PASSED (err u205))
(define-constant ERR_NOT_COMPLIANCE_OFFICER (err u206))
(define-constant ERR_INVALID_ENTITY (err u207))

;; Data variables
(define-data-var next-compliance-id uint u1)
(define-data-var compliance-fee uint u500000)

;; Map to store compliance requirements for entities
(define-map entity-compliance-requirements
  { entity-id: uint, compliance-id: uint }
  {
    requirement-type: (string-ascii 50),     ;; LICENSE, CERTIFICATION, FILING, AUDIT
    requirement-name: (string-ascii 100),
    description: (string-ascii 300),
    issuing-authority: (string-ascii 100),
    status: (string-ascii 20),               ;; PENDING, COMPLIANT, EXPIRED, VIOLATED
    priority: (string-ascii 10),             ;; HIGH, MEDIUM, LOW
    deadline-block: uint,
    reminder-blocks: uint,                   ;; Blocks before deadline to remind
    compliance-evidence: (buff 32),
    created-at: uint,
    updated-at: uint,
    created-by: principal
  }
)

;; Map to track compliance officers for entities
(define-map entity-compliance-officers
  { entity-id: uint, officer: principal }
  {
    appointed-at: uint,
    appointed-by: principal,
    permissions: (string-ascii 20),          ;; FULL, READ_ONLY, SPECIFIC
    is-active: bool
  }
)

;; Map to store compliance notifications and alerts
(define-map compliance-alerts
  { entity-id: uint, compliance-id: uint }
  {
    alert-type: (string-ascii 20),          ;; DEADLINE_WARNING, OVERDUE, VIOLATION
    alert-message: (string-ascii 200),
    severity: (string-ascii 10),            ;; CRITICAL, WARNING, INFO
    triggered-at: uint,
    acknowledged: bool,
    acknowledged-by: (optional principal),
    acknowledged-at: (optional uint)
  }
)

;; Map to track compliance history
(define-map compliance-history
  { entity-id: uint, compliance-id: uint, entry-id: uint }
  {
    previous-status: (string-ascii 20),
    new-status: (string-ascii 20),
    notes: (string-ascii 300),
    evidence-hash: (buff 32),
    updated-by: principal,
    updated-at: uint
  }
)

;; Map to count compliance requirements per entity
(define-map entity-compliance-count
  { entity-id: uint }
  { count: uint, compliant-count: uint, overdue-count: uint }
)

;; Add a compliance requirement to an entity
(define-public (add-compliance-requirement
  (entity-id uint)
  (requirement-type (string-ascii 50))
  (requirement-name (string-ascii 100))
  (description (string-ascii 300))
  (issuing-authority (string-ascii 100))
  (priority (string-ascii 10))
  (deadline-blocks uint)
  (reminder-blocks uint))
  (let
    (
      (compliance-id (var-get next-compliance-id))
      (current-block stacks-block-height)
      (deadline-block (+ current-block deadline-blocks))
    )
    ;; Verify entity exists by calling main contract
    (asserts! (contract-call? .Legidex is-entity-owner entity-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (>= (stx-get-balance tx-sender) (var-get compliance-fee)) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq priority "HIGH") (is-eq priority "MEDIUM") (is-eq priority "LOW")) ERR_INVALID_PRIORITY)
    
    (try! (stx-transfer? (var-get compliance-fee) tx-sender CONTRACT_OWNER))
    
    (map-set entity-compliance-requirements
      { entity-id: entity-id, compliance-id: compliance-id }
      {
        requirement-type: requirement-type,
        requirement-name: requirement-name,
        description: description,
        issuing-authority: issuing-authority,
        status: "PENDING",
        priority: priority,
        deadline-block: deadline-block,
        reminder-blocks: reminder-blocks,
        compliance-evidence: 0x00,
        created-at: current-block,
        updated-at: current-block,
        created-by: tx-sender
      }
    )
    
    ;; Update entity compliance count
    (match (map-get? entity-compliance-count { entity-id: entity-id })
      existing-count
        (map-set entity-compliance-count
          { entity-id: entity-id }
          { 
            count: (+ (get count existing-count) u1),
            compliant-count: (get compliant-count existing-count),
            overdue-count: (get overdue-count existing-count)
          })
      (map-set entity-compliance-count
        { entity-id: entity-id }
        { count: u1, compliant-count: u0, overdue-count: u0 })
    )
    
    (var-set next-compliance-id (+ compliance-id u1))
    (ok compliance-id)
  )
)

;; Update compliance status with evidence
(define-public (update-compliance-status
  (entity-id uint)
  (compliance-id uint)
  (new-status (string-ascii 20))
  (evidence-hash (buff 32))
  (notes (string-ascii 300)))
  (let
    (
      (requirement (unwrap! (map-get? entity-compliance-requirements { entity-id: entity-id, compliance-id: compliance-id }) ERR_NOT_FOUND))
      (current-block stacks-block-height)
      (is-officer (is-some (map-get? entity-compliance-officers { entity-id: entity-id, officer: tx-sender })))
      (is-owner (contract-call? .Legidex is-entity-owner entity-id tx-sender))
    )
    (asserts! (or is-owner is-officer) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq new-status "PENDING") (is-eq new-status "COMPLIANT") 
                  (is-eq new-status "EXPIRED") (is-eq new-status "VIOLATED")) ERR_INVALID_STATUS)
    
    ;; Record status change in history
    (map-set compliance-history
      { entity-id: entity-id, compliance-id: compliance-id, entry-id: current-block }
      {
        previous-status: (get status requirement),
        new-status: new-status,
        notes: notes,
        evidence-hash: evidence-hash,
        updated-by: tx-sender,
        updated-at: current-block
      }
    )
    
    ;; Update the requirement
    (map-set entity-compliance-requirements
      { entity-id: entity-id, compliance-id: compliance-id }
      (merge requirement {
        status: new-status,
        compliance-evidence: evidence-hash,
        updated-at: current-block
      })
    )
    
    ;; Update compliance counts
    (let
      (
        (current-count (default-to { count: u0, compliant-count: u0, overdue-count: u0 } 
                                   (map-get? entity-compliance-count { entity-id: entity-id })))
        (status-change (if (and (not (is-eq (get status requirement) "COMPLIANT")) (is-eq new-status "COMPLIANT")) 1
                          (if (and (is-eq (get status requirement) "COMPLIANT") (not (is-eq new-status "COMPLIANT"))) -1 0)))
      )
      (map-set entity-compliance-count
        { entity-id: entity-id }
        {
          count: (get count current-count),
          compliant-count: (if (> status-change 0) 
                             (+ (get compliant-count current-count) u1)
                             (if (< status-change 0) 
                               (- (get compliant-count current-count) u1)
                               (get compliant-count current-count))),
          overdue-count: (get overdue-count current-count)
        })
    )
    
    (ok true)
  )
)

;; Appoint a compliance officer for an entity
(define-public (appoint-compliance-officer
  (entity-id uint)
  (officer principal)
  (permissions (string-ascii 20)))
  (let
    (
      (current-block stacks-block-height)
    )
    (asserts! (contract-call? .Legidex is-entity-owner entity-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq permissions "FULL") (is-eq permissions "READ_ONLY") (is-eq permissions "SPECIFIC")) ERR_INVALID_STATUS)
    
    (map-set entity-compliance-officers
      { entity-id: entity-id, officer: officer }
      {
        appointed-at: current-block,
        appointed-by: tx-sender,
        permissions: permissions,
        is-active: true
      }
    )
    
    (ok true)
  )
)

;; Generate compliance alert based on deadline proximity
(define-public (check-compliance-deadlines (entity-id uint) (compliance-id uint))
  (let
    (
      (requirement (unwrap! (map-get? entity-compliance-requirements { entity-id: entity-id, compliance-id: compliance-id }) ERR_NOT_FOUND))
      (current-block stacks-block-height)
      (deadline-block (get deadline-block requirement))
      (reminder-blocks (get reminder-blocks requirement))
      (warning-block (- deadline-block reminder-blocks))
    )
    (asserts! (or (contract-call? .Legidex is-entity-owner entity-id tx-sender)
                  (is-some (map-get? entity-compliance-officers { entity-id: entity-id, officer: tx-sender }))) ERR_UNAUTHORIZED)
    
    ;; Check if deadline has passed
    (if (>= current-block deadline-block)
      (begin
        (map-set compliance-alerts
          { entity-id: entity-id, compliance-id: compliance-id }
          {
            alert-type: "OVERDUE",
            alert-message: "Compliance requirement is overdue",
            severity: "CRITICAL",
            triggered-at: current-block,
            acknowledged: false,
            acknowledged-by: none,
            acknowledged-at: none
          }
        )
        ;; Auto-update status to EXPIRED if still pending
        (if (is-eq (get status requirement) "PENDING")
          (map-set entity-compliance-requirements
            { entity-id: entity-id, compliance-id: compliance-id }
            (merge requirement { status: "EXPIRED", updated-at: current-block }))
          true
        )
      )
      ;; Check if warning period has been reached
      (if (>= current-block warning-block)
        (map-set compliance-alerts
          { entity-id: entity-id, compliance-id: compliance-id }
          {
            alert-type: "DEADLINE_WARNING",
            alert-message: "Compliance deadline approaching",
            severity: (if (is-eq (get priority requirement) "HIGH") "CRITICAL" "WARNING"),
            triggered-at: current-block,
            acknowledged: false,
            acknowledged-by: none,
            acknowledged-at: none
          }
        )
        true
      )
    )
    
    (ok true)
  )
)

;; Acknowledge a compliance alert
(define-public (acknowledge-alert
  (entity-id uint)
  (compliance-id uint))
  (let
    (
      (alert (unwrap! (map-get? compliance-alerts { entity-id: entity-id, compliance-id: compliance-id }) ERR_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (or (contract-call? .Legidex is-entity-owner entity-id tx-sender)
                  (is-some (map-get? entity-compliance-officers { entity-id: entity-id, officer: tx-sender }))) ERR_UNAUTHORIZED)
    
    (map-set compliance-alerts
      { entity-id: entity-id, compliance-id: compliance-id }
      (merge alert {
        acknowledged: true,
        acknowledged-by: (some tx-sender),
        acknowledged-at: (some current-block)
      })
    )
    
    (ok true)
  )
)

;; Remove compliance officer
(define-public (remove-compliance-officer
  (entity-id uint)
  (officer principal))
  (let
    (
      (officer-info (unwrap! (map-get? entity-compliance-officers { entity-id: entity-id, officer: officer }) ERR_NOT_FOUND))
    )
    (asserts! (contract-call? .Legidex is-entity-owner entity-id tx-sender) ERR_UNAUTHORIZED)
    
    (map-set entity-compliance-officers
      { entity-id: entity-id, officer: officer }
      (merge officer-info { is-active: false })
    )
    
    (ok true)
  )
)

;; Update compliance fee (owner only)
(define-public (update-compliance-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set compliance-fee new-fee)
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-compliance-requirement (entity-id uint) (compliance-id uint))
  (map-get? entity-compliance-requirements { entity-id: entity-id, compliance-id: compliance-id })
)

(define-read-only (get-compliance-officer (entity-id uint) (officer principal))
  (map-get? entity-compliance-officers { entity-id: entity-id, officer: officer })
)

(define-read-only (get-compliance-alert (entity-id uint) (compliance-id uint))
  (map-get? compliance-alerts { entity-id: entity-id, compliance-id: compliance-id })
)

(define-read-only (get-compliance-history (entity-id uint) (compliance-id uint) (entry-id uint))
  (map-get? compliance-history { entity-id: entity-id, compliance-id: compliance-id, entry-id: entry-id })
)

(define-read-only (get-entity-compliance-summary (entity-id uint))
  (map-get? entity-compliance-count { entity-id: entity-id })
)

(define-read-only (get-next-compliance-id)
  (var-get next-compliance-id)
)

(define-read-only (get-compliance-fee)
  (var-get compliance-fee)
)

(define-read-only (is-compliance-officer (entity-id uint) (user principal))
  (match (map-get? entity-compliance-officers { entity-id: entity-id, officer: user })
    officer-info (get is-active officer-info)
    false
  )
)

(define-read-only (is-deadline-approaching (entity-id uint) (compliance-id uint))
  (match (map-get? entity-compliance-requirements { entity-id: entity-id, compliance-id: compliance-id })
    requirement 
      (let
        (
          (current-block stacks-block-height)
          (deadline-block (get deadline-block requirement))
          (reminder-blocks (get reminder-blocks requirement))
          (warning-block (- deadline-block reminder-blocks))
        )
        (and (>= current-block warning-block) (< current-block deadline-block))
      )
    false
  )
)

(define-read-only (is-overdue (entity-id uint) (compliance-id uint))
  (match (map-get? entity-compliance-requirements { entity-id: entity-id, compliance-id: compliance-id })
    requirement 
      (>= stacks-block-height (get deadline-block requirement))
    false
  )
)

(define-read-only (get-compliance-status (entity-id uint) (compliance-id uint))
  (match (map-get? entity-compliance-requirements { entity-id: entity-id, compliance-id: compliance-id })
    requirement 
      (if (and (not (is-eq (get status requirement) "COMPLIANT")) 
               (>= stacks-block-height (get deadline-block requirement)))
        "OVERDUE"
        (get status requirement))
    "NOT_FOUND"
  )
)
