function __fish_az_resource_groups
	az group list | jq -r .[].name
end

function __fish_az_resource_uris
	az resource list | jq -r .[].id
end

complete -c az --long-option resource-uri --require-parameter --no-files --arguments '(__fish_az_resource_uris)'
complete -c az --short-option g --long-option resource-group --require-parameter --no-files --arguments '(__fish_az_resource_groups)'
complete -c az -f -a '(__fish_argcomplete_complete az)'
