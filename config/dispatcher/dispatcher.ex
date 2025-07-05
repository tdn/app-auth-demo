defmodule Dispatcher do
  use Matcher
  define_accept_types [
    html: [ "text/html", "application/xhtml+html" ],
    json: [ "application/json", "application/vnd.api+json" ],
    sparql: [ "application/sparql-results+json" ]
  ]

  @any %{}
  @json %{ accept: %{ json: true }, layer: :services }
  @html %{ accept: %{ html: true } }

  define_layers [ :static, :frontend_html, :sparql, :services, :fall_back, :not_found ]

  ###############
  # Frontend
  ###############
  match "/.well-known/*path", _ do
    send_resp( conn, 200, "{ \"message\": \"ok\"}" )
  end

  get "/favicon.ico/*path", _ do
    send_resp( conn, 404, "No icon specified" )
  end

  get "/assets/*path", %{ layer: :static } do
    Proxy.forward conn, path, "http://frontend/assets/"
  end

  get "/*_", %{ accept: [:html], layer: :frontend_html } do
    Proxy.forward conn, [], "http://frontend/index.html"
  end

  head "/*_", %{ accept: [:html], layer: :frontend_html } do
    Proxy.forward conn, [], "http://frontend/index.html"
  end

  ###############
  # Mock login
  ###############
  match "/sessions/*path", @json do
    forward conn, path, "http://login/sessions/"
  end

  get "/mock/sessions/*path" do
    forward conn, path, "http://login/sessions/"
  end

  match  "/mock/sessions/*path", @json do
    forward conn, path, "http://login/sessions/"
  end

  match "/accounts/*path", @json do
    Proxy.forward conn, path, "http://resource/accounts/"
  end

  match "/users/*path", @json do
    Proxy.forward conn, path, "http://resource/users/"
  end

  match "/memberships/*path", @json do
    Proxy.forward conn, path, "http://resource/memberships/"
  end


  match "/organizations/*path", @json do
    Proxy.forward conn, path, "http://resource/organizations/"
  end


  ###############
  # SPARQL
  ###############
  match "/sparql", %{ layer: :sparql, accept: %{ sparql: true } } do
    forward conn, [], "http://database:8890/sparql"
  end

  # In order to forward the 'themes' resource to the
  # resource service, use the following forward rule:
  #
  # match "/themes/*path", @json do
  #   Proxy.forward conn, path, "http://resource/themes/"
  # end
  #
  # Run `docker-compose restart dispatcher` after updating
  # this file.

  match "/books/*path", @json do
    Proxy.forward conn, path, "http://resource/books/"
  end

  match "/*_", %{ layer: :not_found } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
