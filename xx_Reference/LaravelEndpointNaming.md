## Laravel Endpoint Naming

Actions handled By resource controller:
|Verb|URI|Action|Route Name|
|--|--|--|--|
|`GET`|`/photos`|`index`|`photos.index`|
|`GET`|`/photos/create`|`create`|`photos.create`|
|`POST`|`/photos`|`store`|`photos.store`|
|`GET`|`/photos/{photo}`|`show`|`photos.show`|
|`GET`|`/photos/{photo}/edit`|`edit`|`photos.edit`|
|`PUT/PATCH`|`/photos/{photo}`|`update`|`photos.update`|
|`DELETE`|`/photos/{photo}`|`destroy`|`photos.destroy`|
