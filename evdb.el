;;; evdb.el --- EVDB API client for Emacs

;; Copyright (C) 2005, 2006  EVDB, Inc.

;; Author: Edward O'Connor <ted@evdb.com>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; evdb.el is an EVDB API client library for Emacs.

;;; Code:

(require 'md5)
(require 'url)      ;; Tested with the URL package in CVS Emacs
(require 'xml)      ;; xml.el in CVS Emacs

;; Placate the byte-compiler.
(defvar url-http-end-of-headers)

;; * User-serviceable parts.

(defgroup evdb nil
  "Emacs interface to EVDB's REST API."
  :group 'processes
  :prefix "evdb-")

(defcustom evdb-app-key nil
  "Your application's EVDB API key."
  :group 'evdb
  :type '(string))

;; * General elisp utilities

(defun evdb-md5 (string)
  "Return a lower-case MD5sum of STRING."
  (downcase (md5 string)))

(defun evdb-string (object)
  "Return a string representation of OBJECT."
  (cond ((stringp object) object)
        ((symbolp object) (symbol-name object))
        ((numberp object) (number-to-string object))
        (t                (format "%s" object))))

(defun evdb-format-url-parameters (alist)
  "Format ALIST as HTTP query parameters.

\(evdb-format-url-parameters '((foo . 1) (bar . \"baz quux\")))
    => \"foo=1&bar=baz+quux\""
  (mapconcat
   (lambda (cons)
     (format "%s=%s"
             (url-hexify-string (evdb-string (car cons)))
             (url-hexify-string (evdb-string (cdr cons)))))
   alist
   "&"))

(defun evdb-xml-node-text (xml &rest path)
  "Extract the text of the node described by PATH in XML."
  (let ((node xml))
    (while path
      (setq node (car (xml-get-children node (pop path)))))
    (mapconcat 'identity (xml-node-children node) " ")))

;; * Bleah.

(defvar evdb-user-key nil)

;; * HTTP request/response handling

(put 'evdb-error 'error-message "EVDB API error")
(put 'evdb-error 'error-conditions '(evdb-error error))

(defvar evdb-debug nil)

;; http://api.evdb.com/docs/errors
(defun evdb-check-error (response)
  "Check for an error in RESPONSE.
If an error is found, signal the error."
  (let ((string (xml-get-attribute response 'string))
        (description
         (evdb-xml-node-text response 'description)))
    (when (eq (xml-node-name response) 'error)
      (when evdb-debug
        (switch-to-buffer (current-buffer)))
      (signal 'evdb-error (list string description)))))

(defun evdb-response (buffer &optional no-error)
  "Process the XML response from EVDB which resides in BUFFER.
If optional argument NO-ERROR is non-nil, don't check for errors in the
response."
  (unwind-protect
      (with-current-buffer buffer
        (save-excursion
          (goto-char url-http-end-of-headers)
          (let ((response (xml-parse-region (point) (point-max) nil nil nil)))
            (setq response (car response))
            (unless no-error
              (evdb-check-error response))
            response)))
    (unless evdb-debug
      (kill-buffer buffer))))

(defvar evdb-username nil
  "The username under which you're currently logged in.")

(defun evdb-request (call args &optional http-method no-error)
  "Perform a EVDB API request to CALL with ARGS using HTTP-METHOD.
If optional argument NO-ERROR is non-nil, don't check for errors in the
response."
  (let ((url-package-name "evdb.el")
        (url-request-method (or http-method "GET")))
    (setq args (cons (cons 'app_key evdb-app-key) args))
    (when evdb-user-key
      (setq args (append `((user . ,evdb-username)
                           (user_key . ,evdb-user-key))
                         args)))
    (evdb-response
     (url-retrieve-synchronously
      (let ((url (concat "http://api.evdb.com/rest" call "?"
                         (evdb-format-url-parameters args))))
        (message "evdb-equest: %s" url)
        url))
     no-error)))

;; * Authentication

(defun evdb-login (username password)
  "Login as USERNAME with PASSWORD.

Read more here: http://api.evdb.com/docs/auth"
  (let* ((nonce
          (evdb-xml-node-text
           (let ((evdb-username nil)
                 (evdb-user-key nil))
             ;; We directly call `evdb-request' here because
             ;; `evdb-api/users/login won't do what we'd like it to.
             (evdb-request "/users/login" nil nil t))
           'nonce))
         (response (evdb-md5 (concat nonce ":" (evdb-md5 password)))))
    (setq evdb-username username)
    (setq evdb-user-key
          (evdb-xml-node-text
           (evdb-api/users/login username nonce response)
           'user_key))))

;; * Tools for defining API method wrappers.

(defmacro evdb-api-method (method required-args optional-args docstring
                           &rest body)
  "Define an elisp binding for EVDB API method METHOD.

Specify REQUIRED-ARGS and OPTIONAL-ARGS. Documentation in the form of a
DOCSTRING is required.

The forms of BODY are evaluated before anything else happens."
  (let ((method-name (intern (concat "evdb-api" method)))
        (all-args (append required-args optional-args)))
    `(defun ,method-name (,@required-args
                          ,@(when optional-args
                              (append '(&optional) optional-args)))
       ,(format "%s\n\nRead more here: http://api.evdb.com/docs%s\n"
                docstring method)
       ,@body
       (let ((args (list ,@(mapcar (lambda (arg)
                                     (list 'cons (list 'quote arg) arg))
                                   required-args))))
         ,@(mapcar (lambda (arg)
                     `(when ,arg
                        (push (cons ',arg ,arg) args)))
                   optional-args)
         (evdb-request ,method args)))))
(put 'evdb-api-method 'lisp-indent-function 3)
(put 'evdb-api-method 'doc-string-elt 4)

(defmacro evdb-api-undocumented (method)
  "Define an elisp binding for EVDB API method METHOD.

METHOD is undocumented at <URL:http://api.evdb.com/docs/>, so we blindly
pass through whatever you've passed into this method. Assumes the
arguments are in the form of a plist."
  (let ((method-name (intern (concat "evdb-api" method))))
    `(defun ,method-name (&rest args)
       ,(format (concat "EVDB API method %s is undocumented.\n\n"
                        "ARGS should be a plist of parameter names and values.\n"
                        "e.g., (evdb-api%s \"foo\" 1 \"bar\" 2)\n\n"
                        "Read more here: http://api.evdb.com/docs%s\n")
                method method method)
       (let ((real-args (list)))
         (while props
           (add-to-list real-args (cons (car props) (cadr props)))
           (setq props (cddr props)))
         (evdb-request ,method real-args)))))

;; * Wrapper functions for each API method.



(evdb-api-method "/calendars/delete" (id) ()
  "Delete the calendar whose id is ID.")

(evdb-api-method "/calendars/events/add" (calendar_id event_id) ()
  "Add EVENT_ID to the calendar whose id is CALENDAR_ID.")

(evdb-api-method "/calendars/events/list" (id modified_since)
                 (sort_order sort_direction page_size page_number)
  "List this calendar's events.")

(evdb-api-method "/calendars/events/remove" (calendar_id event_id) ()
  "Remove the event whose id is EVENT_ID from the calendar CALENDAR_ID.")

(evdb-api-method "/calendars/get" (id) ()
  "Fetch the calendar identified by ID.")

(evdb-api-undocumented "/calendars/images/add")
(evdb-api-undocumented "/calendars/images/remove")

(evdb-api-method "/calendars/modify" (id)
                 (calendar_name description tags privacy where_query
                                what_query notify_schedule)
  "Modify the calendar with id ID.")

(evdb-api-method "/calendars/new" (calendar_name)
                 (description tags privacy where_query what_query
                              notify_schedule)
  "Create a new calendar named CALENDAR_NAME.")

(evdb-api-method "/calendars/properties/add" (id name value) ()
  "Add the property NAME=VALUE to the calendar ID.")

(evdb-api-method "/calendars/properties/list" (id) ()
  "List the properties of the calendar with id ID.")

(evdb-api-method "/calendars/properties/remove" (id) (property_id name)
  "Remove a property from the calendar whose id is ID."
  (assert (or property_id name) nil
          "Must provide either `property_id' or `name'"))

(evdb-api-undocumented "/calendars/rights/add")
(evdb-api-undocumented "/calendars/rights/remove")

(evdb-api-method "/calendars/search" ()
                 (keywords count_only sort_order sort_direction page_size
                           page_number)
  "Search for calendars.")



(evdb-api-method "/categories/get" (id) ()
  "Fetch the category identified by ID.")
(evdb-api-method "/categories/list" () ()
  "List EVDB's categories.")



(evdb-api-undocumented "/demands/comments/delete")
(evdb-api-undocumented "/demands/comments/modify")
(evdb-api-undocumented "/demands/comments/new")
(evdb-api-undocumented "/demands/events/add")
(evdb-api-undocumented "/demands/events/remove")

(evdb-api-method "/demands/get" (id) ()
  "Fetch the demand identified by ID.")

(evdb-api-undocumented "/demands/images/add")
(evdb-api-undocumented "/demands/images/remove")
(evdb-api-undocumented "/demands/links/delete")
(evdb-api-undocumented "/demands/links/new")
(evdb-api-undocumented "/demands/locales/search")

(evdb-api-method "/demands/modify" (id) (description)
  "Modify the demand identified by ID.")

(evdb-api-method "/demands/new" (performer_id location) (description tags)
  "Create a demand for PERFORMER_ID in LOCATION.")

(evdb-api-undocumented "/demands/restore")

(evdb-api-method "/demands/search" ()
                 (keywords location count_only sort_order sort_direction
                           page_size page_number)
  "Search for demands.")

(evdb-api-undocumented "/demands/tags/add")
(evdb-api-undocumented "/demands/tags/list")
(evdb-api-undocumented "/demands/tags/remove")

(evdb-api-method "/demands/withdraw" (id note) ()
  "Withdraw the demand with id ID, for reason NOTE.")



(evdb-api-undocumented "/events/categories/add")
(evdb-api-undocumented "/events/categories/remove")

(evdb-api-method "/events/comments/delete" (comment_id) ()
  "Delete the comment with id COMMENT_ID.")

(evdb-api-method "/events/comments/modify" (comment_id, comment) ()
  "Modify the comment with id COMMENT_ID. COMMENT is the new text.")

(evdb-api-method "/events/comments/new" (id comment) ()
  "Add a comment to the event with id ID. COMMENT is the comment text.")

(evdb-api-undocumented "/events/flags/add")
(evdb-api-undocumented "/events/flags/list")
(evdb-api-undocumented "/events/flags/remove")

(evdb-api-method "/events/get" (id) ()
  "Fetch the event with identified by ID.")

(evdb-api-method "/events/going/list" (id) ()
  "Get a list of users who are going to this event.")

(evdb-api-method "/events/images/add" (id image_id) ()
  "Add the image whose id is IMAGE_ID to the event with id ID.")

(evdb-api-undocumented "/events/images/delete")
(evdb-api-undocumented "/events/images/new")

(evdb-api-method "/events/images/remove" (id image_id) ()
  "Remove the image whose id is IMAGE_ID to the event with id ID.")

(evdb-api-method "/events/links/delete" (link_id) ()
  "Delete the link with id LINK_ID from the event.")

(evdb-api-method "/events/links/new" (id link link_type_id) (description)
  "Add a new link to the event with id ID.
LINK is the URL, and LINK_TYPE_ID must be one of the values in the
documentation.")

(evdb-api-method "/events/modify" (id)
                 (title start_time stop_time tz_olson_path all_day
                        description privacy tags free price venue_id parent_id)
  "Modify the event with id ID.")

(evdb-api-method "/events/new" (title start_time)
                 (stop_time tz_olson_path all_day description privacy
                            tags free price venue_id parent_id)
  "Create a new event.")

(evdb-api-method "/events/performers/add" (id performer_id) ()
  "Add the performer with id PERFORMER_ID to the event with id ID.")

(evdb-api-undocumented "/events/performers/list")

(evdb-api-method "/events/performers/remove" (id performer_id) ()
  "Remove the performer with id PERFORMER_ID from the event with id ID.")

(evdb-api-method "/events/properties/add" (id name value) ()
  "Add the property NAME=VALUE to the event with id ID.")

(evdb-api-method "/events/properties/list" (id) ()
  "List the properties of the event whose id is ID.")

(evdb-api-method "/events/properties/remove" (id) (property_id name)
  "Remove a property from the event whose id is ID."
  (assert (or property_id name) nil
          "Must provide either `property_id' or `name'"))

(evdb-api-undocumented "/events/recurrence/list")

(evdb-api-method "/events/restore" (id) ()
  "Restore the event with identified by ID.")

(evdb-api-method "/events/rights/add" (id relation) ()
  "Enables RELATION (friends/family/contacts) to edit the event with id ID.")

(evdb-api-method "/events/rights/remove" (id realtion) ()
  "Disables RELATION (friends/family/contacts) from editing the event with ID.")

(evdb-api-method "/events/search" ()
                 (keywords location date within units count_only
                           sort_order sort_direction page_size page_number)
  "Search for events.")

(evdb-api-undocumented "/events/tags/add")

(evdb-api-method "/events/tags/delete" (tags id) ()
  "Delete TAGS from the event whose id is ID.")

(evdb-api-method "/events/tags/list" (id) ()
  "Fetch the tags of the event identified by ID.")

(evdb-api-method "/events/tags/new" (tags id) ()
  "Add TAGS to the event whose id is ID.")

(evdb-api-undocumented "/events/tags/remove")

(evdb-api-method "/events/tags/search" (tag) ()
  "Search for events tagged TAG.")

(evdb-api-method "/events/withdraw" (id) (note)
  "Withdraw the event identified by ID.
NOTE, if non-null, should be a string explaining the withdrawal.")



(evdb-api-undocumented "/groups/calendars/add")
(evdb-api-undocumented "/groups/calendars/delete")
(evdb-api-undocumented "/groups/comments/add")
(evdb-api-undocumented "/groups/comments/list")
(evdb-api-undocumented "/groups/comments/modify")
(evdb-api-undocumented "/groups/comments/remove")

(evdb-api-method "/groups/events/add" (id event_id) ()
  "Add EVENT_ID to the group whose id is ID.")

(evdb-api-undocumented "/groups/events/list")
(evdb-api-undocumented "/groups/events/remove")

(evdb-api-method "/groups/get" (id) ()
  "Fetch the group identified by ID.")

(evdb-api-undocumented "/groups/images/add")
(evdb-api-undocumented "/groups/images/remove")
(evdb-api-undocumented "/groups/links/add")
(evdb-api-undocumented "/groups/links/list")
(evdb-api-undocumented "/groups/links/remove")
(evdb-api-undocumented "/groups/modify")
(evdb-api-undocumented "/groups/new")
(evdb-api-undocumented "/groups/search")
(evdb-api-undocumented "/groups/tags/add")
(evdb-api-undocumented "/groups/tags/list")
(evdb-api-undocumented "/groups/tags/remove")
(evdb-api-undocumented "/groups/users/add")
(evdb-api-undocumented "/groups/users/delete")

(evdb-api-method "/groups/users/list" (id) ()
  "Fetches the list of members of the group whose id is ID.")

(evdb-api-undocumented "/groups/users/remove")
(evdb-api-undocumented "/groups/venues/add")
(evdb-api-undocumented "/groups/venues/delete")
(evdb-api-method "/groups/withdraw" (id note) ()
  "Withdraw the group whose id is ID, with reason NOTE.")



(evdb-api-undocumented "/images/delete")
(evdb-api-undocumented "/images/list")

(evdb-api-method "/images/new" () (image_file image_url caption)
  "Upload an image to EVDB."
  (assert (or image_file image_url) nil
          "Must provide either `image_file' or `image_url'"))



(evdb-api-method "/links/types/list" () ()
  "Returns the list of link types supported by EVDB.")



(evdb-api-undocumented "/locales/search")



(evdb-api-method "/metros/upcoming/search" (venue_id) ()
  "Given a venue ID, returns a list of Upcoming.org metros the venue
might possibly be in.")



(evdb-api-undocumented "/performers/comments/delete")
(evdb-api-undocumented "/performers/comments/modify")
(evdb-api-undocumented "/performers/comments/new")
(evdb-api-undocumented "/performers/demands/list")
(evdb-api-undocumented "/performers/events/list")

(evdb-api-method "/performers/get" (id) ()
  "Fetch the performer identified by ID.")

(evdb-api-method "/performers/images/add" (id image_id) ()
  "Add IMAGE_ID to performer whose id is ID.")

(evdb-api-method "/performers/images/remove" (id image_id) ()
  "Remove IMAGE_ID from performer whose id is ID.")

(evdb-api-undocumented "/performers/links/add")
(evdb-api-undocumented "/performers/links/remove")

(evdb-api-method "/performers/modify" (id) (name short_bio long_bio tags)
  "Edit the performer whose id is ID.")

(evdb-api-method "/performers/new" (name short_bio) (long_bio tags)
  "Create a new performer, named NAME.")

(evdb-api-undocumented "/performers/restore")

(evdb-api-method "/performers/search" ()
                 (keywords count_only sort_order sort_direction page_size
                  page_number)
  "Search for performers.")

(evdb-api-undocumented "/performers/tags/add")
(evdb-api-undocumented "/performers/tags/list")
(evdb-api-undocumented "/performers/tags/remove")
(evdb-api-undocumented "/performers/user/add")
(evdb-api-undocumented "/performers/user/remove")

(evdb-api-method "/performers/withdraw" (id note) ()
  "Withdraw the performer whose id is ID, with reason NOTE.")



(evdb-api-undocumented "/users/calendars/delete")

(evdb-api-method "/users/calendars/events/list" (id)
                 (page_size page_number interval offset)
  "Given a calendar ID, returns a list of its events.")

(evdb-api-method "/users/calendars/get" (id) ()
  "Returns the properties of calendar ID.")

(evdb-api-method "/users/calendars/list" (owner) ()
  "Returns a list of OWNER's calendars.")

(evdb-api-undocumented "/users/demands/list")
(evdb-api-undocumented "/users/events/list")
(evdb-api-undocumented "/users/events/recent")
(evdb-api-undocumented "/users/favorites/add")
(evdb-api-undocumented "/users/favorites/check")
(evdb-api-undocumented "/users/favorites/list")
(evdb-api-undocumented "/users/favorites/remove")

(evdb-api-method "/users/get" (username) ()
  "Fetch the user named USERNAME.")

(evdb-api-method "/users/going/add" (event_id) ()
  "Marks the logged in user as going to EVENT_ID.")

(evdb-api-method "/users/going/remove" (event_id) ()
  "Removes the logged in user from EVENT_ID's going list.")

(evdb-api-undocumented "/users/groups/calendars/list")

(evdb-api-method "/users/groups/list" (id) ()
  "Lists the groups of which the user ID is a member.")

(evdb-api-undocumented "/users/groups/users")
(evdb-api-undocumented "/users/images/add")
(evdb-api-undocumented "/users/images/delete")
(evdb-api-undocumented "/users/images/get")
(evdb-api-undocumented "/users/images/new")
(evdb-api-undocumented "/users/images/remove")

(evdb-api-method "/users/locales/add" (id locale) ()
  "Adds LOCALE to the user (ID)'s list of saved locales.")

(evdb-api-method "/users/locales/delete" (id locale) ()
  "Removes LOCALE from the user (ID)'s list of saved locales.")

(evdb-api-method "/users/locales/list" (id) ()
  "Returns a list of the user (ID)'s locales.")

(evdb-api-method "/users/login" (user nonce response) ()
  "Log in.

THIS IS NOT THE USER-SERVICEABLE LOGIN INTERFACE. Please call
`evdb-login' instead.

Read more here: http://api.evdb.com/docs/auth")

(evdb-api-undocumented "/users/prefs/delete")
(evdb-api-undocumented "/users/prefs/get")
(evdb-api-undocumented "/users/prefs/put")

(evdb-api-method "/users/relations/add" (member_id relation) ()
  "Adds MEMBER_ID as bearing RELATION to the logged-in user.")

(evdb-api-method "/users/relations/list" (id members) ()
  "Lists all relations associated with the user (whose id is ID).

If members is non-null, returns the members of each relation as well.")

(evdb-api-method "/users/relations/remove" (member_id relation) ()
  "Removes MEMBER_ID from the logged in user's RELATION list.")

(evdb-api-method "/users/search" (keywords)
                 (sort_order count_only page_size page_number)
  "Searches for users.")

(evdb-api-method "/users/venues/list" (id) ()
  "Lists the venues added by the user whose id is ID.")



(evdb-api-method "/venues/comments/delete" (comment_id) ()
  "Delete the comment with id COMMENT_ID.")

(evdb-api-method "/venues/comments/modify" (comment_id, comment) ()
  "Modify the comment with id COMMENT_ID. COMMENT is the new text.")

(evdb-api-method "/venues/comments/new" (id comment) ()
  "Add a comment to the venue with id ID. COMMENT is the comment text.")

(evdb-api-method "/venues/get" (id) ()
  "Fetch the venue identified by ID.")

(evdb-api-undocumented "/venues/images/add")
(evdb-api-undocumented "/venues/images/delete")
(evdb-api-undocumented "/venues/images/new")
(evdb-api-undocumented "/venues/images/remove")

(evdb-api-method "/venues/links/delete" (link_id) ()
  "Delete the link with id LINK_ID from the venue.")

(evdb-api-method "/venues/links/new" (id link link_type_id) (description)
  "Add a new link to the venue with id ID.
LINK is the URL, and LINK_TYPE_ID must be one of the values in the
documentation.")

(evdb-api-method "/venues/modify" (id country venue_type)
                 (name address city region postal_code description
                       privacy parent_id)
  "Modify the venue whose id is ID.")

(evdb-api-method "/venues/new" (name country venue_type)
                 (address city region postal_code description privacy
                          url url_type parent_id)
  "Create a new venue named NAME.")

(evdb-api-method "/venues/properties/add" (id name value) ()
  "Add the property NAME=VALUE to the venue ID.")

(evdb-api-method "/venues/properties/list" (id) ()
  "List the properties of the venue with id ID.")

(evdb-api-method "/venues/properties/remove" (id) (property_id name)
  "Remove a property from the venue whose id is ID."
  (assert (or property_id name) nil
          "Must provide either `property_id' or `name'"))

(evdb-api-method "/venues/restore" (id) ()
  "Restore the venue whose id is ID.")

(evdb-api-undocumented "/venues/rights/add")
(evdb-api-undocumented "/venues/rights/remove")

(evdb-api-method "/venues/search" (keywords)
                 (location count_only page_size page_number)
  "Search for venues.")

(evdb-api-method "/venues/tags/delete" (tags id) ()
  "Delete TAGS from the venue whose id is ID.")

(evdb-api-method "/venues/tags/list" (id) ()
  "Fetch the tags of the venue identified by ID.")

(evdb-api-method "/venues/tags/new" (tags id) ()
  "Add TAGS to the venue whose id is ID.")

(evdb-api-method "/venues/tags/search" (tag) (page_size page_number)
  "List venues associated with TAG.")

(evdb-api-method "/venues/withdraw" (id note) ()
  "Withdraw the venue whose id is ID, with reason NOTE.")

(provide 'evdb)
;;; evdb.el ends here
