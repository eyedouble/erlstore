
# Erlstore



## Persistence status codes

| Code          | Type          | Message|
| ------------- |:-------------:| :-----:|
| 2000          | Ok            | General success status |
| 4000          | Error         | General error status |
| 4001          | Error         | Resource does not excist |
| 4002          | Error         | Cannot create duplicate |
| 4003          | Error         | Cannot update resource without ID |
| 4010          | Error - Table | Cannot create on reserved table name |
| 4020          | Error - Domain| Create domain: object does not meet requirements |
| 4030          | Error - User  | Create user: object does not meet requirements |
| 4031          | Error - User  | Create user: domain does not excist |
| 5000          | Authorisation | General authorisation status |
| 5001          | Authorisation | User not authorised to perform action |


## Key value store (CommonCRUD)
Read section ![Tables](#tables) if you did not create a table yet.

### GetAll
### Get
### Create
### Update
### Delete
### Filter
You can filter by any (nested) property on your document.
See the table below for operators.

`persistence:filter ( Table, Filters, User )`.

Filters must be a list of tuples, a filter tuple consists of property, operator, value. 
See the example below.

`persistence:filter ( Table, [{ <<"_system.created">>, <<">=">>, 1519138939 }], User )`

Example object that would match:

`{
    "id": "c6a007de-4b82-4008-9b5c-d009e55782ae",
    "name": "kak3",
    "_system": {
        "access": "superadmin",
        "created": 1519138939,
        "last_editor": "superadmin",
        "owner": "superadmin",
        "updated": 1519138939
    }            
}`

#### Operators
| Operator      | Description              |
| ------------- |:------------------------:| 
| isset         | Is set                   | 
| isnotset      | Is not set               | 
| =             | Exactly equal to         | 
| /             | Exactly not equal to     | 
| <             | Exactly equal to         | 
| =<            | Exactly equal to         | 
| >             | Greater than             | 




## Tables
### Create
### Delete

## Domains
### Create
The domain object must contain a `name` and `groups` property.
Name must be of type string.
Groups is of type list of strings.

Eg.

`{ "name":"Example domain", "groups":["admin", "guests"] }`

### Delete

## Users
### Create
The user object must contain an `id` and `domain` property.
Id must be of type string and is both the id as well as the username for that user.
Domain is a combination of the `domain` id and `domain group list position` seperated by a `:`. This property is used for Access Control.

Eg.

`6a3e55d2-5344-4226-a136-a6cf18d3bccc:0`


### Delete


## Access Control
Access control in Erlstore is done on a per document basis.


### Using AC, or not
All functions of the API have a AC variant and one without.

Eg. With and without AC respectively.

`persistence:get ( movies, <<"abf18cca-fe78-48b7-a465-b00ae8d9fc44">>, UserObject )`

`persistence:get ( movies, <<"abf18cca-fe78-48b7-a465-b00ae8d9fc44">> )`


> ## Don't drink too much of the cool-aid
>
> If you use create or update functions without AC these documents are created or updated with
> SuperAdmin permissions. Hence 'normal' users cannot access them.
> 
> Using non AC variant of the API is like typing `sudo` in your terminal.
> It's not bad, but it carries extra responsibility!
>

