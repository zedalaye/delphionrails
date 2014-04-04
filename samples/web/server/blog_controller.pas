unit blog_controller;

interface
uses application_controller, superobject, mypool;

type
  TBlogController = class(TApplicationController)
  public
    procedure Index_get(out data: ISuperObject);
    procedure new_post(const data: TBlog);
    procedure view_get(id: Integer; out data: ISuperObject);
    procedure edit_get(id: Integer; out data: ISuperObject);
    procedure edit_post(const data: TBlog);
    procedure delete_post(id: Integer);
  end;

implementation

{ TBlogController }

procedure TBlogController.edit_post(const data: TBlog);
begin
  if data.validate then
    with pool.Connection.Transaction do
     Execute('update blog set title = ?, body = ? where id = ?',
       [data.title, data.body, data.id]);
end;

procedure TBlogController.delete_post(id: Integer);
begin
  with pool.Connection.Transaction do
    Execute('delete from blog where id = ?', [id]);
  Redirect('blog', 'index');
end;

procedure TBlogController.edit_get(id: Integer; out data: ISuperObject);
begin
  with pool.Connection.Transaction do
    data := Singleton('select * from blog where id = ?', [id]);
end;

procedure TBlogController.Index_get(out data: ISuperObject);
begin
  with pool.Connection.Transaction do
    data := Execute('select title, id, post_date from blog order by post_date');
end;

procedure TBlogController.new_post(const data: TBlog);
begin
  if data.validate then
    with pool.Connection.Transaction do
     Redirect(Singleton('insert into blog (title, body) values (?, ?) returning id',
       [data.title, data.body]).Format('/blog/view/%id%'));
end;

procedure TBlogController.view_get(id: Integer; out data: ISuperObject);
begin
  with pool.Connection.Transaction do
    data := Singleton('select * from blog where id = ?', [id]);
  if data = nil then
    SetErrorCode(404);
end;

initialization
  TBlogController.Register;

end.
