function ici --description 'show info about current repo and tools'
	ici_init

	switch $argv[1]
	case 'env'
	ici_env

	case 'git'
	ici_git
	
	case 'node'
	ici_node
	
	case 'npm'
	ici_npm

	case 'yarn'
	ici_yarn
	
	case 'docker'
	ici_docker
	
	case 'package'
	ici_package

	case 'typescript'
	ici_typescript

	case 'prettier'
	ici_prettier
	
	case 'eslint'
	ici_eslint

	case 'jest'
	ici_jest

	case '*'
	ici_env
	ici_git
	ici_node
	ici_npm
	ici_yarn
	ici_docker

	ici_package

	ici_typescript
	ici_prettier
	ici_eslint
	ici_jest
	end
end

function ici_init
	set --global ici_git_repo false

	if type -q git
		if git status >/dev/null 2>&1
			set --global ici_git_repo true
		end
	end
end

function ici_env
	set_color --bold
	printf "\nEnv - https://www.dotenv.org\n"
	set_color normal

	fd --hidden --no-ignore --exclude .git --exclude node_modules --type file '\.env'

	if test $ici_git_repo = true
		if test -f package.json
			set --local ici_dotenv_desired_version (jq --raw-output .devDependencies.dotenv package.json)
			printf "📦 desired $ici_dotenv_desired_version\n"
		end
	end
end

function ici_git
	set_color --bold
	printf "\nGit - https://git-scm.com\n"
	set_color normal
	printf "📚 https://git-scm.com/doc\n"

	if type -q git
		set --local ici_git_latest_version (curl --silent 'https://archlinux.org/packages/extra/x86_64/git/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $ici_git_latest_version - https://archlinux.org/packages/extra/x86_64/git/\n"

		# git version x.y.z
		set --local ici_git_global_version (git --version | string sub --start 13)
		printf "✅ global $ici_git_global_version\n"
	else
		printf "❌ global none\n"
	end

	if git status >/dev/null 2>&1
		printf "✅ in a repo\n"
		set --global ici_git_repo true

		fd --hidden --exclude .git --type file '.gitignore'
	else
		printf "❌ not in a repo\n"
		set --global ici_git_repo false
	end
end

function ici_node
	set_color --bold
	printf "\nNode.js - https://nodejs.org\n"
	set_color normal
	printf "📚 https://nodejs.org/dist/latest-v18.x/docs/api/\n"

	if type -q node
		set --local ici_node_latest_version (curl --silent 'https://archlinux.org/packages/community/x86_64/nodejs/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $ici_node_latest_version - https://archlinux.org/packages/community/x86_64/nodejs/\n"

		# vx.y.z
		set --local ici_node_global_version (node --version | string sub --start 2)
		printf "✅ global $ici_node_global_version\n"
	else
		printf "❌ global none\n"
	end

	if test $ici_git_repo = true
		if test -f .nvmrc
			printf "⚙️ .nvmrc\n"
		end
	end
end

function ici_npm
	set_color --bold
	printf "\nNpm - https://npmjs.com\n"
	set_color normal
	printf "📚 https://docs.npmjs.com\n"

	if type -q npm
		set --local ici_npm_latest_version (curl --silent 'https://archlinux.org/packages/community/any/npm/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $ici_npm_latest_version - https://archlinux.org/packages/community/any/npm/\n"

		# x.y.z		
		set --local ici_npm_global_version (npm --version)
		printf "✅ global $ici_npm_global_version\n"
	else
		printf "❌ global none\n"
	end
end

function ici_yarn
	set_color --bold
	printf "\nYarn - https://yarnpkg.com\n"
	set_color normal
	printf "📚 https://yarnpkg.com/getting-started\n"

	if type -q yarn
		set --local ici_yarn_latest_version (curl --silent 'https://archlinux.org/packages/community/any/yarn/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $ici_yarn_latest_version - https://archlinux.org/packages/community/any/yarn/\n"
		# x.y.z
		set --local ici_yarn_global_version (yarn --version)
		set --local ici_yarn_global_path (command -v yarn)
		printf "✅ global $ici_yarn_global_version - $ici_yarn_global_path\n"
	else
		printf "❌ global none\n"
	end
end

function ici_docker
	set_color --bold
	printf "\nDocker - https://www.docker.com\n"
	set_color normal
	printf "📚 https://docs.docker.com\n"

	if test $ici_git_repo = true
		fd --hidden --exclude .git --type file 'Dockerfile'
		fd --hidden --exclude .git --type file 'docker-compose'
		fd --hidden --exclude .git --type file 'dockerignore'
	end
end

function ici_package
	set_color --bold
	printf "\nPackage\n"
	set_color normal

	if test -f package.json
		set --local ici_package_name (jq --raw-output .name package.json)
		set --local ici_package_version (jq --raw-output .version package.json)
		set --local ici_package_workspaces (jq --raw-output .workspaces package.json)

		printf "📦 $ici_package_name@$ici_package_version\n"

		if test "$ici_package_workspaces" != null
			printf "✅ workspaces\n"
		else
			printf "❌ workspaces\n"
		end
	end
end

function ici_typescript
	set_color --bold
	printf "\nTypeScript - https://www.typescriptlang.org\n"
	set_color normal
	printf "📚 https://www.typescriptlang.org/docs/\n"

	if type -q tsc
		set --local ici_tsc_latest_arch_version (curl --silent 'https://archlinux.org/packages/community/any/typescript/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $ici_tsc_latest_arch_version - https://archlinux.org/packages/community/any/typescript/\n"
		
		set --local ici_tsc_latest_version (curl --silent 'https://registry.npmjs.com/typescript' | jq --raw-output '.["dist-tags"].latest')
		printf "🌍 latest $ici_tsc_latest_version - https://registry.npmjs.com/typescript\n"

		# Version x.y.z
		set --local ici_tsc_global_version (tsc --version | string sub --start=9)
		printf "✅ global $ici_tsc_global_version\n"
	else
		printf "❌ global none\n"
	end

	if test $ici_git_repo = true
		set --local ici_tsc_reported_version (npx tsc --version | string sub --start=9)
		printf "✅ local  $ici_tsc_reported_version\n"

		if test -f package.json
			set --local ici_typescript_desired_version (jq --raw-output .devDependencies.typescript package.json)
			printf "📦 desired $ici_typescript_desired_version\n"

			set --local ici_typescript_eslint_desired_version (jq --raw-output '.devDependencies["@typescript-eslint/parser"]' package.json)
			printf "📦 @typescript-eslint/parser $ici_typescript_eslint_desired_version\n"
		end

		for config in $(fd --strip-cwd-prefix --hidden --exclude .git --type file 'tsconfig');
			echo "$config $(jq .extends $config 2> /dev/null || echo 'INVALID JSON (extra commas…)')"
		end
	end
end

function ici_prettier
	set_color --bold
	printf "\nPrettier - https://prettier.io\n"
	set_color normal
	printf "📚 https://prettier.io/docs/en/index.html\n"

	set --local ici_prettier_latest_version (curl --silent 'https://registry.npmjs.com/prettier' | jq --raw-output '.["dist-tags"].latest')
	printf "🌍 latest $ici_prettier_latest_version - https://registry.npmjs.com/prettier\n"

	if type -q prettier
		# x.y.z
		set --local ici_prettier_global_version (prettier --version)
		printf "✅ global $ici_prettier_global_version\n"
	else
		printf "❌ global none\n"
	end

	if test $ici_git_repo = true
		set --local ici_prettier_reported_version (npx prettier --version)
		printf "✅ local  $ici_prettier_reported_version\n"

		if test -f package.json
			set --local ici_prettier_desired_version (jq --raw-output .devDependencies.prettier package.json)
			printf "📦 desired $ici_prettier_desired_version\n"

			set --local ici_prettier_eslint_config_desired_version (jq --raw-output '.devDependencies["eslint-config-prettier"]' package.json)
			if test "$ici_prettier_eslint_config_desired_version" != null
				printf "📦 eslint-config-prettier $ici_prettier_eslint_config_desired_version\n"
			end

			set --local ici_prettier_eslint_plugin_desired_version (jq --raw-output '.devDependencies["eslint-plugin-prettier"]' package.json)
			if test "$ici_prettier_eslint_plugin_desired_version" != null
				printf "📦 eslint-plugin-prettier $ici_prettier_eslint_plugin_desired_version\n"
			end

			set --local ici_prettier_package_config (jq --raw-output .prettier package.json)
			if test "$ici_prettier_package_config" != null
				printf "⚙️ package.json\n"
			end
		end

		# https://prettier.io/docs/en/configuration.html
		fd --hidden --exclude .git --type file 'prettierrc'
		fd --hidden --exclude .git --type file 'prettierignore'
	end
end

function ici_eslint
	set_color --bold
	printf "\nESLint - https://eslint.org\n"
	set_color normal
	printf "📚 https://eslint.org/docs/user-guide/\n"

	set --local ici_eslint_latest_version (curl --silent 'https://registry.npmjs.com/eslint' | jq --raw-output '.["dist-tags"].latest')
	printf "🌍 latest $ici_eslint_latest_version - https://registry.npmjs.com/eslint\n"

	if type -q eslint
		# vx.y.z
		set --local ici_eslint_global_version (eslint --version | string sub --start=2)
		printf "✅ global $ici_eslint_global_version\n"
	else
		printf "❌ global none\n"
	end

	if test $ici_git_repo = true
		set --local ici_eslint_reported_version (npx eslint --version | string sub --start=2)
		printf "✅ local  $ici_eslint_reported_version\n"

		if test -f package.json
			set --local ici_eslint_desired_version (jq --raw-output .devDependencies.eslint package.json)
			printf "📦 desired $ici_eslint_desired_version\n"

			set --local ici_typescript_eslint_desired_version (jq --raw-output '.devDependencies["@typescript-eslint/parser"]' package.json)
			printf "📦 @typescript-eslint/parser $ici_typescript_eslint_desired_version\n"
		end

		# https://eslint.org/docs/user-guide/configuring/
		for config in $(fd --strip-cwd-prefix --hidden --exclude .git --type file 'eslintrc');
			echo "$config $(jq .extends $config 2> /dev/null || echo 'INVALID JSON (extra commas…)')"
		end

		fd --hidden --exclude .git --type file 'eslintignore'
	end
end

function ici_jest
	set_color --bold
	printf "\nJest - https://jestjs.io\n"
	set_color normal
	printf "📚 https://jestjs.io/docs/getting-started\n"

	set --local ici_jest_latest_version (curl --silent 'https://registry.npmjs.com/jest' | jq --raw-output '.["dist-tags"].latest')
	printf "🌍 latest $ici_jest_latest_version - https://registry.npmjs.com/jest\n"

	if type -q jest
		# x.y.z
		set --local ici_jest_global_version (jest --version)
		printf "✅ global $ici_jest_global_version\n"
	else
		printf "❌ global none\n"
	end

	if test $ici_git_repo = true
		set --local ici_jest_reported_version (npx jest --version)
		printf "✅ local $ici_jest_reported_version\n"

		if test -f package.json
			set --local ici_jest_desired_version (jq --raw-output .devDependencies.jest package.json)
			printf "📦 jest $ici_jest_desired_version\n"

			set --local ici_type_jest_desired_version (jq --raw-output '.devDependencies["@types/jest"]' package.json)
			printf "📦 @types/jest $ici_type_jest_desired_version\n"

			set --local ici_ts_jest_desired_version (jq --raw-output '.devDependencies["ts-jest"]' package.json)
			printf "📦 ts-jest $ici_ts_jest_desired_version\n"
		end

		fd --hidden --exclude .git --type file 'jest\.config'
	end
end

