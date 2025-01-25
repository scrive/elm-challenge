module Style exposing (..)


type Button
    = AddNewTag
    | SubmitNewTag
    | SubmitValueForRestoredTag
    | UpdateTagValue
    | SubmitTagValue
    | ArchiveTag
    | RemoveTag
    | RestoreTag
    | AddPolicy
    | SetContactValue
    | UpdateTimeout
    | SubmitTimeout
    | RemoveTimeout


altText b =
    case b of
        AddNewTag ->
            "[Add New Tag]"

        SubmitNewTag ->
            "[Submit New Tag]"

        UpdateTagValue ->
            "[Update Tag Value]"

        SubmitTagValue ->
            "[Submit Value]"

        SubmitValueForRestoredTag ->
            "[Restore Tag With Given Value]"

        ArchiveTag ->
            "[Archive Tag]"

        RemoveTag ->
            "[Remove Tag]"

        RestoreTag ->
            "[Restore Tag]"

        AddPolicy ->
            "[Add Policy]"

        SetContactValue ->
            "[Set Value]"

        UpdateTimeout ->
            "[Update Timeout]"

        SubmitTimeout ->
            "[Submit Timeout]"

        RemoveTimeout ->
            "[Remove Timeout]"


buttonLabel b =
    case b of
        AddNewTag ->
            "+"

        -- ➕
        SubmitNewTag ->
            "☑"

        UpdateTagValue ->
            "↻"

        SubmitTagValue ->
            "☑"

        SubmitValueForRestoredTag ->
            "☑"

        ArchiveTag ->
            "❖"

        RemoveTag ->
            "—"

        RestoreTag ->
            "↷"

        AddPolicy ->
            "+"

        SetContactValue ->
            "☑"

        UpdateTimeout ->
            "↻"

        SubmitTimeout ->
            "☑"

        RemoveTimeout ->
            "—"


textForSelectPolicy =
    "<Select Policy>"


mainContainer =
    "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48"


formHeader =
    "p-2 m-4 mt-6 text-3xl font-bold text-transparent bg-clip-text bg-gradient-to-br from-slate-600 to-slate-800"


textInput =
    "border-slate-300 border-solid border-2 rounded-r-md mr-3"


gradientHeader =
    "p-2 text-5xl font-extrabold text-transparent bg-clip-text bg-gradient-to-br from-slate-400 to-slate-800"


subheader =
    "p-2 text-2xl font-extrabold text-slate-800"


codeBox =
    "my-8 py-4 px-9 text-xs bg-slate-100 font-mono shadow rounded"


inputLabel =
    "text-gray-900 font-semibold py-2 mr-2"


errorsList =
    ""


errorItem =
    "border-red-400 rounded-sm"


itemWithInput =
    "py-2"


itemWithValue =
    "py-2"


itemWithEditableValue =
    "py-2"


selectBox =
    "text-blue-600 border-3 border-indigo font-semibold mr-2"


selectOption =
    "font-semibold"


selectEmptyOption =
    ""


checkBox =
    "mr-2"


button =
    "text-blue-700 hover:text-white border border-blue-700 hover:bg-blue-200 focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-md px-3 py-1.5 text-center me-2 mb-2 dark:border-blue-500 dark:text-blue-500 dark:hover:text-white dark:hover:bg-blue-500 dark:focus:ring-blue-800 disabled:border-slate-400"


readOnlyValuesList =
    "list-none"


inputsForValuesList =
    "list-none"


fieldLabel =
    "text-gray-900 font-semibold py-2 mr-2"


fieldValue =
    "text-blue-700 mr-3"


tagList =
    "list-none"


tagItem =
    "mx-0 my-3"


editingTagItem =
    "list-none"


archivedTagItem =
    "list-none"


tagName =
    "text-blue-600 bg-slate-200 px-1.5 py-1 rounded-l-md rounded-r-none"


archivedTagName =
    "text-teal-600 bg-slate-100 px-1.5 py-1 rounded-l-md rounded-r-md mr-3"


tagValue =
    "text-white bg-slate-500 px-1.5 py-1 rounded-l-none rounded-r-md mr-3"
