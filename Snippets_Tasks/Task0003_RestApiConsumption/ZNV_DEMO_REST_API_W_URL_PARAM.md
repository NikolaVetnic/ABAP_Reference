# Consuming a REST API with ABAP

```ABAP
*&---------------------------------------------------------------------*
*& Report znv_demo_rest_api_w_url_param
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT znv_demo_rest_api_w_url_param.

SELECTION-SCREEN BEGIN OF BLOCK url WITH FRAME TITLE TEXT-001.
  PARAMETERS:
      p_url TYPE string.
SELECTION-SCREEN END OF BLOCK url.

START-OF-SELECTION.

  " STEP 1 : Create an HTTP Client Object
  cl_http_client=>create_by_url(
    EXPORTING
      url                    = p_url
    IMPORTING
      client                 = DATA(lo_http)
    EXCEPTIONS
      argument_not_found     = 1
      plugin_not_active      = 2
      internal_error         = 3
      OTHERS                 = 7
  ).

  IF sy-subrc = 0.
    " STEP 2 : Make a Request
    DATA(co_timeout_default) = 15.

    lo_http->send(
      EXPORTING
        timeout                    = co_timeout_default
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).

    " STEP 3 : Ask for a Response
    lo_http->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4
    ).

    " STEP 4 : Get the Data
    DATA(result) = lo_http->response->get_cdata( ).

    " STEP 5 : Display the Data
    cl_abap_browser=>show_html(
      EXPORTING
        title        = 'Rest API Consumption DEMO'
        html_string  = result
    ).

  ENDIF.
```

Tutorial detailing the example can be found [here](http://nikolapacekvetnic.rs/?p=1700).
