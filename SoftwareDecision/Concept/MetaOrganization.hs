module SoftwareDecision.Concept.MetaOrganization where

-- the structure/organization of the metadata
dvcsName = "dvcs"
dvcsPath = "." ++ dvcsName

repoMetaPath = dvcsPath ++ "/repometadata.json"

objectRelativePath = "snapshot"
objectPath = "." ++ dvcsName ++ "/" ++ objectRelativePath

metaRelativePath = "info"
metaPath = "." ++ dvcsName ++ "/" ++ metaRelativePath

tempPath = "./." ++ dvcsName ++ "/temp"
remoteLoc = "./." ++ dvcsName ++ "/temp/remote"

remoteRepoMetaPath = remoteLoc ++ "/" ++ repoMetaPath

-- we can also store the linked list in a separate file 
historyPath = metaPath ++ "/history"