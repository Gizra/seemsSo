module Backend.Entities exposing (..)

import Backend.Restful exposing (EntityId(..))


{-
   The rest of this are the phantom types for each entity we're using.
   This would benefit from some kind of code-generation step, since you
   could generate the code below easily from a list of base types.

   We create the type aliases so that we can just say

       ItemId

   most of the time, rather than the more verbose

       EntityId ItemId

   In fact, here's a simple code-generation process which you can adapt
   to your preferred code-editor. Here's the list of base types:

       Item
       ItemComment

   Now, to create a new one, use the following process:

   1. Add it to the list above.
   2. Delete everything below this comment.
   3. Copy the list above below this comment.
   4. Use search-and-replace to generate the code.

   For vim, here's the pattern I use:

   s/ *\(.*\)/type alias \1Id = EntityId \1IdType\rtype \1IdType=\1IdType/

   Then, just save it and let elm-format make it pretty.

   Eventually, you could imagine just having a file with a list
   of our entity types, and a little script to generate the code below.
-}


type alias ItemId =
    EntityId ItemIdType


type ItemIdType
    = ItemIdType


type alias ItemCommentId =
    EntityId ItemCommentIdType


type ItemCommentIdType
    = ItemCommentIdType
