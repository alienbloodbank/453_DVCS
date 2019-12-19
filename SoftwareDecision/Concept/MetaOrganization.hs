module SoftwareDecision.Concept.MetaOrganization (
dvcsName,
historyPath,
remoteRepoMetaPath,
remoteLoc,
tempPath,
dvcsPath,
repoMetaPath,
objectRelativePath,
objectPath,
metaRelativePath,
metaPath) where

-- the structure/organization of the metadata
dvcsName = "dvcs"
dvcsPath = "." ++ dvcsName

repoMetaPath = dvcsPath ++ "/repometadata.json"

objectRelativePath = "snapshot"
objectPath = "." ++ dvcsName ++ "/" ++ objectRelativePath

metaRelativePath = "info"
metaPath = "." ++ dvcsName ++ "/" ++ metaRelativePath

tempPath = "./." ++ dvcsName ++ "/temp"
remoteLoc = "./." ++ dvcsName ++ "/temp/remote__"

remoteRepoMetaPath = remoteLoc ++ "/" ++ repoMetaPath

-- we can also store the linked list in a separate file
historyPath = metaPath ++ "/history"
