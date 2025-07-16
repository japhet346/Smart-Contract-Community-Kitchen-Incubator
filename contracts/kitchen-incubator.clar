;; ===================================================
;; SMART CONTRACT COMMUNITY KITCHEN INCUBATOR
;; ===================================================
;; A comprehensive system for supporting local food entrepreneurs
;; with shared commercial kitchen space and business development resources

;; ===================================================
;; CONSTANTS AND ERROR CODES
;; ===================================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_PARAMETERS (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_EQUIPMENT_UNAVAILABLE (err u105))
(define-constant ERR_BOOKING_CONFLICT (err u106))
(define-constant ERR_PERMIT_EXPIRED (err u107))
(define-constant ERR_INVALID_STATUS (err u108))
(define-constant ERR_FUTURE_BLOCK (err u109))

;; ===================================================
;; DATA STRUCTURES
;; ===================================================

;; Entrepreneur profile and membership
(define-map entrepreneurs
  { entrepreneur: principal }
  {
    name: (string-utf8 100),
    business-type: (string-utf8 50),
    membership-tier: (string-ascii 20),
    join-date: uint,
    total-hours-used: uint,
    mentor-assigned: (optional principal),
    active: bool
  }
)

;; Equipment inventory and specifications
(define-map equipment
  { equipment-id: uint }
  {
    name: (string-utf8 100),
    type: (string-ascii 30),
    hourly-rate: uint,
    maintenance-due: uint,
    available: bool,
    location: (string-utf8 50)
  }
)

;; Equipment booking system
(define-map bookings
  { booking-id: uint }
  {
    entrepreneur: principal,
    equipment-id: uint,
    start-block: uint,
    end-block: uint,
    status: (string-ascii 20),
    total-cost: uint,
    created-at: uint
  }
)

;; Health permits and compliance tracking
(define-map health-permits
  { permit-id: uint }
  {
    entrepreneur: principal,
    permit-type: (string-ascii 30),
    issued-date: uint,
    expiry-date: uint,
    status: (string-ascii 20),
    inspector-notes: (string-utf8 200)
  }
)

;; Market access opportunities
(define-map market-opportunities
  { opportunity-id: uint }
  {
    title: (string-utf8 100),
    description: (string-utf8 300),
    organizer: principal,
    event-date: uint,
    application-deadline: uint,
    max-participants: uint,
    current-participants: uint,
    requirements: (string-utf8 200),
    active: bool
  }
)

;; Market opportunity applications
(define-map market-applications
  { application-id: uint }
  {
    opportunity-id: uint,
    entrepreneur: principal,
    application-date: uint,
    status: (string-ascii 20),
    mentor-recommendation: (optional (string-utf8 200))
  }
)

;; Mentorship assignments and tracking
(define-map mentors
  { mentor: principal }
  {
    name: (string-utf8 100),
    expertise: (string-utf8 200),
    max-mentees: uint,
    current-mentees: uint,
    active: bool,
    rating: uint
  }
)

;; Business development resources
(define-map resources
  { resource-id: uint }
  {
    title: (string-utf8 100),
    type: (string-ascii 30),
    description: (string-utf8 300),
    access-level: (string-ascii 20),
    created-by: principal,
    created-at: uint
  }
)

;; ===================================================
;; VARIABLES
;; ===================================================

(define-data-var next-equipment-id uint u1)
(define-data-var next-booking-id uint u1)
(define-data-var next-permit-id uint u1)
(define-data-var next-opportunity-id uint u1)
(define-data-var next-application-id uint u1)
(define-data-var next-resource-id uint u1)

;; System configuration
(define-data-var kitchen-open bool true)
(define-data-var base-membership-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var blocks-per-hour uint u144) ;; Approximate blocks per hour

;; ===================================================
;; ADMIN FUNCTIONS
;; ===================================================

(define-public (add-equipment (name (string-utf8 100)) (type (string-ascii 30)) (hourly-rate uint) (location (string-utf8 50)))
  (let ((equipment-id (var-get next-equipment-id)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR_INVALID_PARAMETERS)
    (asserts! (> hourly-rate u0) ERR_INVALID_PARAMETERS)

    (map-set equipment
      { equipment-id: equipment-id }
      {
        name: name,
        type: type,
        hourly-rate: hourly-rate,
        maintenance-due: (+ stacks-block-height u14400), ;; 100 days from now
        available: true,
        location: location
      }
    )

    (var-set next-equipment-id (+ equipment-id u1))
    (ok equipment-id)
  )
)

(define-public (add-mentor (mentor principal) (name (string-utf8 100)) (expertise (string-utf8 200)) (max-mentees uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR_INVALID_PARAMETERS)
    (asserts! (> max-mentees u0) ERR_INVALID_PARAMETERS)

    (map-set mentors
      { mentor: mentor }
      {
        name: name,
        expertise: expertise,
        max-mentees: max-mentees,
        current-mentees: u0,
        active: true,
        rating: u5
      }
    )

    (ok true)
  )
)

(define-public (create-market-opportunity
  (title (string-utf8 100))
  (description (string-utf8 300))
  (event-date uint)
  (application-deadline uint)
  (max-participants uint)
  (requirements (string-utf8 200)))
  (let ((opportunity-id (var-get next-opportunity-id)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> (len title) u0) ERR_INVALID_PARAMETERS)
    (asserts! (> event-date stacks-block-height) ERR_INVALID_PARAMETERS)
    (asserts! (> application-deadline stacks-block-height) ERR_INVALID_PARAMETERS)
    (asserts! (< application-deadline event-date) ERR_INVALID_PARAMETERS)

    (map-set market-opportunities
      { opportunity-id: opportunity-id }
      {
        title: title,
        description: description,
        organizer: tx-sender,
        event-date: event-date,
        application-deadline: application-deadline,
        max-participants: max-participants,
        current-participants: u0,
        requirements: requirements,
        active: true
      }
    )

    (var-set next-opportunity-id (+ opportunity-id u1))
    (ok opportunity-id)
  )
)

(define-public (issue-health-permit (entrepreneur principal) (permit-type (string-ascii 30)) (expiry-date uint) (inspector-notes (string-utf8 200)))
  (let ((permit-id (var-get next-permit-id)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> expiry-date stacks-block-height) ERR_INVALID_PARAMETERS)
    (asserts! (is-some (map-get? entrepreneurs { entrepreneur: entrepreneur })) ERR_NOT_FOUND)

    (map-set health-permits
      { permit-id: permit-id }
      {
        entrepreneur: entrepreneur,
        permit-type: permit-type,
        issued-date: stacks-block-height,
        expiry-date: expiry-date,
        status: "active",
        inspector-notes: inspector-notes
      }
    )

    (var-set next-permit-id (+ permit-id u1))
    (ok permit-id)
  )
)

;; ===================================================
;; ENTREPRENEUR FUNCTIONS
;; ===================================================

(define-public (register-entrepreneur (name (string-utf8 100)) (business-type (string-utf8 50)) (membership-tier (string-ascii 20)))
  (let ((entrepreneur tx-sender))
    (asserts! (is-none (map-get? entrepreneurs { entrepreneur: entrepreneur })) ERR_ALREADY_EXISTS)
    (asserts! (> (len name) u0) ERR_INVALID_PARAMETERS)
    (asserts! (> (len business-type) u0) ERR_INVALID_PARAMETERS)
    (asserts! (var-get kitchen-open) ERR_INVALID_STATUS)

    ;; Payment check would go here in production

    (map-set entrepreneurs
      { entrepreneur: entrepreneur }
      {
        name: name,
        business-type: business-type,
        membership-tier: membership-tier,
        join-date: stacks-block-height,
        total-hours-used: u0,
        mentor-assigned: none,
        active: true
      }
    )

    (ok true)
  )
)

(define-public (book-equipment (equipment-id uint) (start-block uint) (duration-hours uint))
  (let (
    (entrepreneur tx-sender)
    (booking-id (var-get next-booking-id))
    (equipment-data (unwrap! (map-get? equipment { equipment-id: equipment-id }) ERR_NOT_FOUND))
    (end-block (+ start-block (* duration-hours (var-get blocks-per-hour))))
    (total-cost (* duration-hours (get hourly-rate equipment-data)))
  )
    (asserts! (is-some (map-get? entrepreneurs { entrepreneur: entrepreneur })) ERR_UNAUTHORIZED)
    (asserts! (get available equipment-data) ERR_EQUIPMENT_UNAVAILABLE)
    (asserts! (>= start-block stacks-block-height) ERR_INVALID_PARAMETERS)
    (asserts! (> duration-hours u0) ERR_INVALID_PARAMETERS)
    (asserts! (<= duration-hours u24) ERR_INVALID_PARAMETERS) ;; Max 24 hours

    ;; Check for booking conflicts
    (asserts! (is-none (get-booking-conflict equipment-id start-block end-block)) ERR_BOOKING_CONFLICT)

    ;; Payment would be handled here in production

    (map-set bookings
      { booking-id: booking-id }
      {
        entrepreneur: entrepreneur,
        equipment-id: equipment-id,
        start-block: start-block,
        end-block: end-block,
        status: "confirmed",
        total-cost: total-cost,
        created-at: stacks-block-height
      }
    )

    (var-set next-booking-id (+ booking-id u1))
    (ok booking-id)
  )
)

(define-public (apply-to-market-opportunity (opportunity-id uint))
  (let (
    (entrepreneur tx-sender)
    (application-id (var-get next-application-id))
    (opportunity-data (unwrap! (map-get? market-opportunities { opportunity-id: opportunity-id }) ERR_NOT_FOUND))
    (entrepreneur-data (unwrap! (map-get? entrepreneurs { entrepreneur: entrepreneur }) ERR_UNAUTHORIZED))
  )
    (asserts! (get active opportunity-data) ERR_INVALID_STATUS)
    (asserts! (< stacks-block-height (get application-deadline opportunity-data)) ERR_INVALID_PARAMETERS)
    (asserts! (< (get current-participants opportunity-data) (get max-participants opportunity-data)) ERR_INVALID_PARAMETERS)

    ;; Check if entrepreneur already applied
    (asserts! (is-none (get-existing-application opportunity-id entrepreneur)) ERR_ALREADY_EXISTS)

    (map-set market-applications
      { application-id: application-id }
      {
        opportunity-id: opportunity-id,
        entrepreneur: entrepreneur,
        application-date: stacks-block-height,
        status: "pending",
        mentor-recommendation: none
      }
    )

    ;; Update participant count
    (map-set market-opportunities
      { opportunity-id: opportunity-id }
      (merge opportunity-data { current-participants: (+ (get current-participants opportunity-data) u1) })
    )

    (var-set next-application-id (+ application-id u1))
    (ok application-id)
  )
)

(define-public (request-mentor-assignment)
  (let (
    (entrepreneur tx-sender)
    (entrepreneur-data (unwrap! (map-get? entrepreneurs { entrepreneur: entrepreneur }) ERR_UNAUTHORIZED))
  )
    (asserts! (is-none (get mentor-assigned entrepreneur-data)) ERR_ALREADY_EXISTS)

    ;; In production, this would trigger a matching algorithm
    ;; For now, we'll just mark as available for assignment
    (map-set entrepreneurs
      { entrepreneur: entrepreneur }
      (merge entrepreneur-data { mentor-assigned: none })
    )

    (ok true)
  )
)

;; ===================================================
;; MENTOR FUNCTIONS
;; ===================================================

(define-public (accept-mentee (entrepreneur principal))
  (let (
    (mentor tx-sender)
    (mentor-data (unwrap! (map-get? mentors { mentor: mentor }) ERR_UNAUTHORIZED))
    (entrepreneur-data (unwrap! (map-get? entrepreneurs { entrepreneur: entrepreneur }) ERR_NOT_FOUND))
  )
    (asserts! (get active mentor-data) ERR_INVALID_STATUS)
    (asserts! (< (get current-mentees mentor-data) (get max-mentees mentor-data)) ERR_INVALID_PARAMETERS)
    (asserts! (is-none (get mentor-assigned entrepreneur-data)) ERR_ALREADY_EXISTS)

    ;; Update mentor
    (map-set mentors
      { mentor: mentor }
      (merge mentor-data { current-mentees: (+ (get current-mentees mentor-data) u1) })
    )

    ;; Update entrepreneur
    (map-set entrepreneurs
      { entrepreneur: entrepreneur }
      (merge entrepreneur-data { mentor-assigned: (some mentor) })
    )

    (ok true)
  )
)

(define-public (provide-market-recommendation (application-id uint) (recommendation (string-utf8 200)))
  (let (
    (mentor tx-sender)
    (application-data (unwrap! (map-get? market-applications { application-id: application-id }) ERR_NOT_FOUND))
    (entrepreneur-data (unwrap! (map-get? entrepreneurs { entrepreneur: (get entrepreneur application-data) }) ERR_NOT_FOUND))
  )
    (asserts! (is-some (map-get? mentors { mentor: mentor })) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get mentor-assigned entrepreneur-data) (some mentor)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status application-data) "pending") ERR_INVALID_STATUS)

    (map-set market-applications
      { application-id: application-id }
      (merge application-data { mentor-recommendation: (some recommendation) })
    )

    (ok true)
  )
)

;; ===================================================
;; READ-ONLY FUNCTIONS
;; ===================================================

(define-read-only (get-entrepreneur (entrepreneur principal))
  (map-get? entrepreneurs { entrepreneur: entrepreneur })
)

(define-read-only (get-equipment (equipment-id uint))
  (map-get? equipment { equipment-id: equipment-id })
)

(define-read-only (get-booking (booking-id uint))
  (map-get? bookings { booking-id: booking-id })
)

(define-read-only (get-health-permit (permit-id uint))
  (map-get? health-permits { permit-id: permit-id })
)

(define-read-only (get-market-opportunity (opportunity-id uint))
  (map-get? market-opportunities { opportunity-id: opportunity-id })
)

(define-read-only (get-market-application (application-id uint))
  (map-get? market-applications { application-id: application-id })
)

(define-read-only (get-mentor (mentor principal))
  (map-get? mentors { mentor: mentor })
)

(define-read-only (get-resource (resource-id uint))
  (map-get? resources { resource-id: resource-id })
)

(define-read-only (is-equipment-available (equipment-id uint) (start-block uint) (end-block uint))
  (match (map-get? equipment { equipment-id: equipment-id })
    equipment-data
    (and
      (get available equipment-data)
      (is-none (get-booking-conflict equipment-id start-block end-block))
    )
    false
  )
)

(define-read-only (get-entrepreneur-permits (entrepreneur principal))
  (let ((permits (list)))
    ;; In production, this would iterate through all permits
    ;; For now, returning empty list as placeholder
    permits
  )
)

(define-read-only (check-permit-validity (entrepreneur principal) (permit-type (string-ascii 30)))
  (let ((current-block stacks-block-height))
    ;; In production, this would check all permits of the type
    ;; For now, returning false as placeholder
    false
  )
)

;; ===================================================
;; HELPER FUNCTIONS
;; ===================================================

(define-private (get-booking-conflict (equipment-id uint) (start-block uint) (end-block uint))
  ;; In production, this would check all existing bookings
  ;; For now, returning none as placeholder
  none
)

(define-private (get-existing-application (opportunity-id uint) (entrepreneur principal))
  ;; In production, this would check all applications
  ;; For now, returning none as placeholder
  none
)

;; ===================================================
;; UTILITY FUNCTIONS
;; ===================================================

(define-public (emergency-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set kitchen-open false)
    (ok true)
  )
)

(define-public (emergency-resume)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set kitchen-open true)
    (ok true)
  )
)

(define-public (update-membership-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-fee u0) ERR_INVALID_PARAMETERS)
    (var-set base-membership-fee new-fee)
    (ok true)
  )
)

;; ===================================================
;; INITIALIZATION
;; ===================================================

;; Initialize system with basic configuration
(begin
  (var-set kitchen-open true)
  (var-set base-membership-fee u1000000) ;; 1 STX
  (var-set blocks-per-hour u144)
)
