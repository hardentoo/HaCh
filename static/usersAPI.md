## GET /

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

-

```javascript
[]
```

- Example of one user

```javascript
[{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"}]
```

- Example of one user, Example of one user

```javascript
[{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"},{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"}]
```

- Example of another user

```javascript
[{"userId":20,"userFirstName":"Benjamin","userLastName":"Pierce"}]
```

- Example of one user, Example of one user, Example of one user

```javascript
[{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"},{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"},{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"}]
```

## GET /docs

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Exq

```
long md file
```

## GET /has

#### Authentication



Clients must supply the following data


#### GET Parameters:

- name
     - **Values**: *Gotlob, Frege, ...*
     - **Description**: First or second User name


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Success

```
We have user Steve Awodey (10) in our DB
```

- Fail 1

```
Cant find user MacLane in our DB
```

- Fail 2

```
Username not specified
```

## GET /logicians

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

-

```javascript
[]
```

- Example of one user

```javascript
[{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"}]
```

- Example of one user, Example of one user

```javascript
[{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"},{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"}]
```

- Example of another user

```javascript
[{"userId":20,"userFirstName":"Benjamin","userLastName":"Pierce"}]
```

- Example of one user, Example of one user, Example of one user

```javascript
[{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"},{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"},{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"}]
```

## GET /physicists

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

-

```javascript
[]
```

- Example of one user

```javascript
[{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"}]
```

- Example of one user, Example of one user

```javascript
[{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"},{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"}]
```

- Example of another user

```javascript
[{"userId":20,"userFirstName":"Benjamin","userLastName":"Pierce"}]
```

- Example of one user, Example of one user, Example of one user

```javascript
[{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"},{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"},{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"}]
```

## GET /user/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) user id

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

-

```javascript
null
```

- Example of one user

```javascript
{"userId":10,"userFirstName":"Per","userLastName":"Martin-Lof"}
```

- Example of another user

```javascript
{"userId":20,"userFirstName":"Benjamin","userLastName":"Pierce"}
```
