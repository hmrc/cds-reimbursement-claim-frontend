# cds-reimbursement-claim-frontend

Frontend microservice providing an HTML UI to claim back import duty and VAT.

- [Business goal](#business-goal)
- [Usage](#usage)
- [Internals](#internal)
- [External connections](#external-connections)
- [Feature flags](#feature-flags)
- [Development](#development)

## Business goal

You can use this service to apply to claim repayments on:
 - overpayments for import charges, these include Customs Duties, Excise Duties, Countervailing Duties and specific other customs duties,
 - rejected goods, if the import is  rejected  as the goods are damaged, defective, or not by contract
 - security deposits

You can use this service for multiple claims (bulk), and be repaid via either Current Month Adjustment (CMA) or by bank account transfer.

## Usage

This service is publicly available under <https://www.tax.service.gov.uk/claim-back-import-duty-vat> URL. 

Access to this service requires:
 - [Government Gateway login](https://www.gov.uk/log-in-register-hmrc-online-services)
 - [Customs Declaration Service subscription](https://www.gov.uk/guidance/get-access-to-the-customs-declaration-service)

## Internals

Internally the service consists of several types of components:
 - http endpoints defined in `conf/*.routes` files
 - action controllers classes
 - services and repositories
 - business domain aggregates (journeys)
 - data model classes
 - forms definitions
 - html templates written in Twirl language
 - third-party http services connectors

Each type of claim is represented by a single business domain aggregate (journey) responsible for maintaining a consistent and valid state of the claim process, and finally generate an output claim object which will be passed to the backend microservice for processing into the TPI05 API request.

## External connections

This service calls the following external services:
 - [CDS-Reimbursement backend microservice](https://github.com/hmrc/cds-reimbursement-claim) for:
   - customs declaration retrieval (ACC14),
   - CDS subscription details e.g. email, XI EORI (SUB09),
   - claim submission (TPI05)
- Auth microservice for authentication and authorisation of the users,
- [Address lookup frontend](https://github.com/hmrc/address-lookup-frontend/blob/main/README.md),
- [Bank account reputation API](https://github.com/hmrc/bank-account-reputation/blob/main/docs/README.md) for bank details validation,
- [Upload customs documents frontend](https://github.com/hmrc/upload-customs-documents-frontend) for uploading the evidence.

## Feature flags

The feature set of the service is controlled by a host of feature flags defined in the [conf/application.conf](https://github.com/hmrc/cds-reimbursement-claim-frontend/blob/main/conf/application.conf#L221-L229):

| flag | description |
|------|-------------|
| features.overpayments_v2 | enables `Claim back import duty and VAT if you have overpaid` journey | 
| features.rejected-goods | enables `Claim back customs charges paid on rejected goods` journey |
| features.securities | enables `Claim back a security deposit` journey |
| features.view-upload | enables `View claims or upload documents` dashboard |
| features.block-subsidies | disables support for subsidies-related features |
| subsidies-for-overpayments | enables subsidies-related features in `Claim back import duty and VAT if you have overpaid` |
| features.subsidies-for-rejected-goods | enables subsidies-related features in `Claim back customs charges paid on rejected goods` |
| features.xi-eori | enables support for displaying declarations containing XI EORI identifiers |
| features.limited-access | enables access only for users on the an allow list of EORIs defined in th `limited-access-eori-csv-base64` property |

## Development

This service is built using [Play Framework](https://www.playframework.com/) and Scala language (https://www.scala-lang.org/).

### Prerequisites
 - [Java 21](https://adoptium.net/)
 - [SBT build tool](https://www.scala-sbt.org/)

### Build and test

    sbt clean compile test

### Run locally

Running this service locally requires multiple other services to be up and running. The best way to achieve that is by using [Service Manager](https://github.com/hmrc/sm2):

    sm --start CDSRC_ALL

### Run locally in a development mode

    sm --start CDSRC_ALL
    sm --stop CDSRC_FRONTEND
    sbt run

## License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").

