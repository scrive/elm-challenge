module Style exposing (..)


type Button
    = AddNewTag
    | SubmitNewTag
    | SubmitValueForRestoredTag
    | UpdateTagValue
    | ArchiveTag
    | RemoveTag
    | RestoreTag
    | AddPolicy
    | SetContactValue
    | UpdateTimeout
    | SubmitTimeout
    | RemoveTimeout


buttonLabel b =
    case b of
        AddNewTag -> "[Add New Tag]"
        SubmitNewTag -> "[Submit New Tag]"
        UpdateTagValue -> "[Update Tag Value]"
        SubmitValueForRestoredTag -> "[Submit Value]"
        ArchiveTag -> "[Archive Tag]"
        RemoveTag -> "[Remove Tag]"
        RestoreTag -> "[Restore Tag]"
        AddPolicy -> "[Add Policy]"
        SetContactValue -> "[Set Value]"
        UpdateTimeout -> "[Update Timeout]"
        SubmitTimeout -> "[Submit Timeout]"
        RemoveTimeout -> "[Remove Timeout]"


textForSelectPolicy = "<Select Policy>"


mainContainer = "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48"


formHeader = "p-2 m-4 mt-6 text-3xl font-bold text-transparent bg-clip-text bg-gradient-to-br from-slate-600 to-slate-800"


textInput = "border-black border-solid border-2"


gradientHeader = "p-2 text-5xl font-extrabold text-transparent bg-clip-text bg-gradient-to-br from-slate-400 to-slate-800"


subheader = "p-2 text-2xl font-extrabold text-slate-800"


codeBox = "my-8 py-4 px-9 text-xs bg-slate-100 font-mono shadow rounded"


inputLabel = ""


errorsList = ""


errorItem = ""


itemWithInput = ""


itemWithValue = ""


itemWithEditableValue = ""


selectBox = ""


selectOption = ""


selectEmptyOption = ""


checkBox = ""


button = ""


readOnlyValuesList = ""


inputsForValuesList = ""


fieldLabel = ""


fieldValue = ""


tagList = ""


tagItem = ""


editingTagItem = ""


archivedTagItem = ""


tagName = ""


archivedTagName = ""


tagValue = ""