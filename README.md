![Erlstore](https://i.imgur.com/pFJWiF4.png)
# Erlstore
The real-time document store build for reliability, 
magically spreading over both memory and disk storage.
A modern database with an intuitive REST API and Erlang API.
Websockets, access control, filtering and a web based control-panel.

**THIS IS AN ALPHA RELEASE Want to contribute? mail vanwel@eyedouble.nl** 

## Document store
Read section *Tables* if you did not create a table yet.
All methods take an optional user map as last argument to enable Access Control filtering.
For consiseness these variations of the API are omitted below.

### GetAll
```erlang 
erlstore_persistence:getAll ( my_table ).
```

### Get
```erlang 
erlstore_persistence:get ( my_table, <<"id">> ).
```

### Create
*object should not contain an ID erlstore will create an UUID automatically. It will be overwritten*
```erlang 
erlstore_persistence:create ( my_table, #{ <<"data">> => <<"my_data">> } ).
```

### Update
```erlang 
erlstore_persistence:update ( my_table, Id, #{ <<"data">> => <<"my_new_data">> } ).
```

### Delete
```erlang 
erlstore_persistence:update ( my_table, Id ).
```

## Filters
You can filter by any (nested) property on your document.
See the table below for operators.

```erlang
erlstore_persistence:filter ( Table, Filters, User ).
```

Filters must be a list of tuples, a filter tuple consists of property, operator, value. 
See the example below.

```erlang
% one filter
erlstore:persistence:filter ( Table, [{ <<"_system.created">>, <<">=">>, 1519138939 }] ).

% multiple filters
erlstore:persistence:filter ( Table, [
    { <<"_system.created">>, <<">=">>, 1519138939 },
    { <<"_system.owner">>, <<"/">>, <<"irsan">>
]).

% filter with AC enabled
erlstore:persistence:filter ( Table, [{ <<"_system.created">>, <<">=">>, 1519138939 }], User ).

```

Example object that would match:

```erlang 
#{
    "id" => "c6a007de-4b82-4008-9b5c-d009e55782ae",
    "name" => "test",
    "_system" => #{
        "access" => "superadmin",
        "created" => 1519138939,
        "last_editor" => "superadmin",
        "owner" => "superadmin",
        "updated" => 1519138939
    }            
}
```

### Operators
| Operator      | Description                                   |
| ------------- |:---------------------------------------------:| 
| isset         | Is set                                        | 
| isnotset      | Is not set                                    | 
| =             | Exactly equal to                              | 
| /             | Exactly not equal to                          | 
| <             | Lower than                                    | 
| =<            | Equal to or lower than                        | 
| >             | Greater than                                  | 
| >=            | Equal to or greater than                      | 
| str*          | String contains                               | 
| fllen         | Flat list length equals                       | 
| fllen<        | Flat list length lower than                   | 
| fllen=<       | Flat list length equal to or lower than       | 
| fllen         | Flat list length greater than                 | 
| fllen>=       | Flat list length equal to or greater than     | 
| flmem         | Flat list has exactly member                  | 



## Tables
### Create
```erlang 
erlstore_persistence:createTable ( my_table ).
```
### Delete
```
erlang erlstore_persistence:deleteTable ( my_table ).
```

## Domains
### Create
The domain object must contain a `name` and `groups` property.
Name must be of type string.
Groups is of type list of strings.

Eg.

```erlang 
#{ <<"name">> => <<"Example domain">>, <<"groups">> => [ <<"admin">>, <<"guests">> ] }
```

### Delete
Not available yet due to AC logic.

## Users
### Create
The user object must contain an `id` and `domain` property.
Id must be of type string and is both the id as well as the username for that user.
Domain is a combination of the `domain` id and `domain group list position` seperated by a `:`. This property is used for Access Control.

Eg.

```erlang 
<<"6a3e55d2-5344-4226-a136-a6cf18d3bccc:0">>
```


### Delete
Not available yet due to AC logic.



## Access Control
Access control in Erlstore is done on a per document basis.


### Using AC, or not
All functions of the API have a AC variant and one without.

Eg. With and without AC respectively.

```erlang 
erlstore_persistence:get ( movies, <<"abf18cca-fe78-48b7-a465-b00ae8d9fc44">>, User )
```

```erlang 
erlstore_persistence:get ( movies, <<"abf18cca-fe78-48b7-a465-b00ae8d9fc44">> )
```



> ## Don't drink too much of the cool-aid
>
> If you use create or update functions without AC these documents are created or updated with
> SuperAdmin permissions. Hence 'normal' users cannot access them.
> 
> Using non AC variant of the API is like typing `sudo` in your terminal.
> It's not bad, but it comes with extra responsibility!
>


## Change feeds
Change feeds allow one to receive events from a particular table by subscribing to it.

### Events
- Create
- Update
- Delete

You will receive messages if something in that table changes.
In case of create and update events one will receive the latest document.
In case of a delete event the last version of the deleted document is passed along.

### Filtering
All filters as described in the section filters above may be used.


### Subscribe
```erlang 
erlstore_persistence:subscribe ( Pid, Table, Filters).
```

As `Pid` is the process that will receive messages and `Table` is the table to subscribe to.
Messages are standard Erlang messages.

#### Structure
```erlang 
{ ChangeType, Table, Data, [OldData] }
```

#### Create
```erlang 
{2101, {create, my_table, #{ <<id>> => <<"my_new_document">>, <<"tests">> => 3 } } }
```

#### Update
```erlang 
{2102, {update, my_table, #{ <<id>> => <<"my_new_document">>, <<"tests">> => 9 }, #{ <<id>> => <<"my_new_document">>, <<"tests">> => 3 } } }
```

#### Delete
```erlang 
{2102, {delete, my_table, <<"my_new_document">> } }
```

## Data dumping
Erlstore can dumping data to a binary file. This is ideal for backup and export/import purposes.

#### dump_export/1
```erlang 
erlstore_persistence:dump_export ( "data/dump.erlstore" ).
```

#### dump_import/1
```erlang 
erlstore_persistence:dump_import ( "data/dump.erlstore" ).
```

**Erlstore's binary dump files are implementation specific. Erlstore uses Erlang Mnesia to store data. Mnesia and thuss Erlstore data is node specific for distribution purposes. This means a data dump made on one node must be altered to be used on another. Erlstore handles this automatically for you. The if such alteration is performed the file will be renamed to `{filename}._nc.{extension}`. Once Erlstore has completed importing a data file it will rename the dat file to `{filename}._imported.{extension}`**

*Please note the functions `dump/2` have been deprecated and removed from the api.*


## Erlstore status codes

| Code          | Type                      | Message                            |
| ------------- |:-------------------------:| :---------------------------------:|
| 2000          | Ok                        | General success status |
| 2100          | Changefeeds - create      | General success status |
| 2101          | Changefeeds - update      | General success status |
| 2102          | Changefeeds - delete      | General success status |
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
