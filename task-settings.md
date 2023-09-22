# Task - Settings 

## Instructions

Your task is to implement a form for Settings part of User group data.

Feel free to use any package or technique you are used to or you would like to try out.

You have a data provided to you as a `JSON` string in `Data.elm` module. They are based on what our [API](https://apidocs.scrive.com/#view-user-group) returns.

## Requirements

The form should only display fields for values that are not null in the incoming request with option to add new ones that are not there yet. Data retention settings are 

- preparation, 
- closed, 
- canceled, 
- timed out, 
- rejected, 
- error

Form should also contain the boolean for immediate trashing value.

If settings are inherited, you should just display values without being able to edit them.

Root user group can't use inheritance.

## Nice to have

- form is styled to follow nice UX/UI design
- is responsive
- is accessible

## Data description

This part of data controls how many days can the document stay in certain state (preparation, closed, canceled, timedout, rejected, error) before it is discarded. Value of `null` means that it is never discarded in this state.

These settings can be also inherited from any user group above.

### Inheritance

User groups live in a tree structure - they can have many children User Groups which can inherit Settings and Contact details. 
User group with `parent_id: null` is the root user group and cannot inherit anything.



