# 📜 Legidex - Legal DAO Registry

> A decentralized registry for legal entities and agreements on the Stacks blockchain

## 🌟 Overview

Legidex is a comprehensive smart contract system that enables DAOs and organizations to register legal entities, create binding agreements, and manage legal relationships on-chain. It provides a transparent, immutable record of legal structures and their associated agreements.

## ✨ Features

- 🏢 **Legal Entity Registration** - Register LLCs, Corporations, DAOs, and Trusts
- 📋 **Agreement Management** - Create, sign, and manage legal agreements
- 🔗 **Entity-Agreement Linking** - Connect entities to their relevant agreements
- ✍️ **Digital Signatures** - Cryptographic signature verification
- ⏰ **Expiry Management** - Time-bound agreements with automatic expiry
- 👥 **DAO Membership** - Join the registry DAO and build reputation
- 💰 **Fee Management** - Configurable registration fees

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Run Clarinet commands to interact with the contract

## 📖 Usage

### Register a Legal Entity

```clarity
(contract-call? .Legidex register-legal-entity 
  "My Company LLC" 
  "LLC" 
  "Delaware" 
  "12345678")
```

### Create a Legal Agreement

```clarity
(contract-call? .Legidex create-legal-agreement
  "Partnership Agreement"
  "Agreement between parties for business partnership"
  0x1234567890abcdef1234567890abcdef12345678
  u52560) ;; ~1 year in blocks
```

### Sign an Agreement

```clarity
(contract-call? .Legidex sign-agreement
  u1
  0xabcdef1234567890abcdef1234567890abcdef12)
```

### Activate an Agreement

```clarity
(contract-call? .Legidex activate-agreement u1)
```

### Link Entity to Agreement

```clarity
(contract-call? .Legidex link-entity-agreement
  u1
  u1
  0xfedcba0987654321fedcba0987654321fedcba09)
```

## 🔍 Read-Only Functions

- `get-legal-entity` - Retrieve entity details
- `get-legal-agreement` - Retrieve agreement details
- `get-entity-agreement` - Check entity-agreement relationships
- `get-agreement-signature` - Verify signatures
- `is-entity-owner` - Check ownership
- `is-agreement-active` - Check agreement status

## 🏗️ Entity Types Supported

- **LLC** - Limited Liability Company
- **CORP** - Corporation
- **DAO** - Decentralized Autonomous Organization
- **TRUST** - Trust Entity

## 📊 Agreement Statuses

- **DRAFT** - Agreement created but not active
- **ACTIVE** - Agreement is live and can be signed
- **EXPIRED** - Agreement has passed its expiry block

## 🛡️ Security Features

- Owner-only functions for sensitive operations
- Fee-based registration to prevent spam
- Time-based agreement expiry
- Signature verification for authenticity
- Status validation for state transitions

## 🔧 Configuration

The contract owner can update:
- Registry fees via `update-registry-fee`
- Entity statuses can be managed by entity owners

## 📝 Error Codes

- `u100` - Unauthorized access
- `u101` - Entity/Agreement not found
- `u102` - Already exists
- `u103` - Invalid status
- `u104` - Invalid entity type
- `u105` - Agreement expired
- `u106` - Not a signatory

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License.

## 🔮 Future Enhancements

- Multi-signature requirements
- Reputation-based governance
- Integration with external legal databases
- Automated compliance checking
- Cross-chain legal entity recognition
