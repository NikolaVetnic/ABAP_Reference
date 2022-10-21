# Object Oriented Way of Sending an Email from ABAP Side

```ABAP
*&---------------------------------------------------------------------*
*& Report znv_email_sending_oo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT znv_email_sending_oo.

*&---------------------------------------------------------------------*
*& Data Declaration
*&---------------------------------------------------------------------*
DATA:
  lo_mime_helper TYPE REF TO cl_gbt_multirelated_service,
  lo_bcs         TYPE REF TO cl_bcs,
  lo_doc_bcs     TYPE REF TO cl_document_bcs,
  lo_recipient   TYPE REF TO if_recipient_bcs,
  lt_soli        TYPE TABLE OF soli,
  ls_soli        TYPE soli,
  lv_status      TYPE bcs_rqst.

*&---------------------------------------------------------------------*
*& Creation of the Mail
*&---------------------------------------------------------------------*

" create the main object of the mail
CREATE OBJECT lo_mime_helper.

" create the mail content
DATA(mail_string) =
  '<!DOCTYPE html>' &&
  '  <html>' &&
  '    <body>' &&
  '      <p>Hello User!</p>' &&
  '      <p>Mail Content</p>' &&
  '    </body>' &&
  '  </html>'.

lt_soli = cl_document_bcs=>string_to_soli( mail_string ).

" set the HTML body of the mail
lo_mime_helper->set_main_html(
    content     = lt_soli
    description = 'Test Email'
  ).

" set the subject of the mail
lo_doc_bcs = cl_document_bcs=>create_from_multirelated(
    i_subject          = 'Test Email from ABAP Program'
    i_importance       = '9' " 1 is highest priority
    i_multirel_service = lo_mime_helper
  ).

lo_bcs = cl_bcs=>create_persistent( ).
lo_bcs->set_document( i_document = lo_doc_bcs ).

" set the email address
lo_recipient = cl_cam_address_bcs=>create_internet_address(
    i_address_string = 'admin@nikolapacekvetnic.rs'
  ).
lo_bcs->add_recipient(
    i_recipient  = lo_recipient
  ).

" change the status
lv_status = 'N'.
lo_bcs->set_status_attributes(
    i_requested_status = lv_status
  ).

*&---------------------------------------------------------------------*
*& Sending the Mail
*&---------------------------------------------------------------------*

TRY.
    lo_bcs->send( ).
    COMMIT WORK.
    WRITE: / 'Email sent!'.
  CATCH cx_bcs INTO DATA(lx_bcs).
    ROLLBACK WORK.
    WRITE: / lx_bcs->get_text( ).
ENDTRY.

*&---------------------------------------------------------------------*
*& Checking the Mail
*&---------------------------------------------------------------------*
*& Mail can be checked by using the SOST TCode. Switch to SENDER tab a-
*& nd type in your username, then click REFRESH. Selecting the row from
*& the table and clicking on DISPLAY button displays the document.
*&---------------------------------------------------------------------*
```

Original article can be found [here](https://blogs.sap.com/2019/10/11/object-oriented-way-of-sending-an-email-from-abap-side./#:~:text=To%20send%20an%20email%20from,if%20we%20revise%20it%20%F0%9F%98%9B%20.).
