# communis-api
REST Database for Angle Resolved XPS fitting parameters (Aanalizer sponsored)

## USAGE EXAMPLES

- Get User
curl -X GET 52.90.217.197:8000/users/0
- New User
curl -X POST --data '{"email":"curl3@mail.com", "password":"pass2", "alias":"2curl", "image_url":"2curl.image.com", "show_email":true
, "date":"2016-02-19T03:26:07Z"}' 52.90.217.197:8000/users/
- Update User
curl -X PUT --data '{"email":"curl3@mail.com", "password":"pass2222", "alias":"2curl", "image_url":"2curl.image.com", "show_email":false, "date":"2016-02-19T03:26:07Z"}' 52.90.217.197:8000/users/curl3@mail.com
- Delete User
curl -X DELETE 52.90.217.197:8000/users/4