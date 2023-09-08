# Task - Contact details 

## Instructions

Your task is to implement a form for Contact details part of User group data.

Feel free to use any package or technique you are used to or you would like to try out.

You have a data provided to you as a `JSON` string in `Data.elm` module. They are based on what our [API](https://apidocs.scrive.com/#view-user-group) returns.

## Requirements

The form should display fields for all values of contact details values.

Values for email and phone should be validated to contain proper format of values.

User group can have one of these means of preferred contact - `post`, `email`, `phone`. 

These restrictions are in play:
  - if `post` is selected then the address is mandatory to be filled
  - if `email` is selected, then the email is mandatory to be filled
  - if `phone` is selected then the phone is mandatory to be filled

If settings are inherited, you should just display values without being able to edit them.

Root user group can't use inheritance.

## Nice to have

- form is styled to follow nice UX/UI design
- is responsive
- is accessible

## Data description

Contact details data are part of the JSON and should be easy to understand what is what.

These settings can be also inherited from any user group above.

### Inheritance

User groups live in a tree structure - they can have many children User Groups which can inherit Settings and Contact details. 
User group with `parent_id: null` is the root user group and cannot inherit anything.



