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

	case 'terraform'
	ici_terraform

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
	ici_terraform

	ici_package

	ici_typescript
	ici_prettier
	ici_eslint
	ici_jest
	end
end

# helpers

function ici_init
	set --global ici_git_repo false

	if type -q git
		if git status >/dev/null 2>&1
			set --global ici_git_repo true
		end
	end
end

function ici_print_header
	set_color --bold cyan
	printf "\n$argv\n"
	set_color normal
end

function ici_print_dev_dependencies
	if test -f package.json
		for package in $argv
			set --local desired_version (jq --raw-output ".devDependencies[\"$package\"]" package.json)
			printf "📦 $package $desired_version\n"
		end
	end
end

function ici_print_version --argument-names latest installed
	if test "$latest" = "$installed"
		printf $installed
	else
		# probably outdated
		set_color red
		printf $installed
		set_color normal
	end
end

function ici_print_config
	for config in $(fd --strip-cwd-prefix --color never --hidden --exclude .git --type file $argv[1])
		printf "⚙️ $config\n"
	end
end

function ici_print_ignore
	for ignore in $(fd --strip-cwd-prefix --color never --hidden --exclude .git --type file $argv[1])
		printf "👁️ $ignore\n"
	end
end

function ici_env
	ici_print_header "Env - https://www.dotenv.org"

	if test $ici_git_repo = true
		if test -f package.json
			set --local desired_version (jq --raw-output .devDependencies.dotenv package.json)
			printf "📦 desired $desired_version\n"
		end
	end

	for config in $(fd --strip-cwd-prefix --color never --hidden --no-ignore --exclude .git --exclude node_modules --type file '\.env')
		printf "⚙️ $config\n"
	end

	printf "\n⚗️ direnv\n"
	direnv status

	printf "\n🔎 'process.env' occurences\n"
	rg --count --color never process.env
end

function ici_git
	ici_print_header "Git - https://git-scm.com"
	printf "📚 https://git-scm.com/doc\n"

	if type -q git
		set --local latest_version (curl --silent 'https://archlinux.org/packages/extra/x86_64/git/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $latest_version - https://archlinux.org/packages/extra/x86_64/git/\n"

		# git version x.y.z
		set --local global_version (git --version | string sub --start 13)
		set --local global_path (command -v git)
		printf "✅ global $(ici_print_version $latest_version $global_version) - $global_path\n"
	else
		printf "❌ global none\n"
	end

	if git status >/dev/null 2>&1
		printf "✅ in a repo\n"
		set --global ici_git_repo true

		ici_print_ignore 'gitignore'
	else
		printf "❌ not in a repo\n"
		set --global ici_git_repo false
	end
end

function ici_node
	ici_print_header "Node.js - https://nodejs.org"
	printf "📚 https://nodejs.org/dist/latest-v18.x/docs/api/\n"

	if type -q node
		set --local latest_version (curl --silent 'https://archlinux.org/packages/community/x86_64/nodejs/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $latest_version - https://archlinux.org/packages/community/x86_64/nodejs/\n"

		# vx.y.z
		set --local global_version (node --version | string sub --start 2)
		set --local global_path (command -v node)
		printf "✅ global $(ici_print_version $latest_version $global_version) - $global_path\n"
	else
		printf "❌ global none\n"
	end

	if test $ici_git_repo = true
		ici_print_config 'nvmrc'
	end
end

function ici_npm
	ici_print_header "Npm - https://npmjs.com"
	printf "📚 https://docs.npmjs.com\n"

	if type -q npm
		set --local latest_version (curl --silent 'https://archlinux.org/packages/community/any/npm/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $latest_version - https://archlinux.org/packages/community/any/npm/\n"

		# x.y.z
		set --local global_version (npm --version)
		set --local global_path (command -v npm)
		printf "✅ global $(ici_print_version $latest_version $global_version) - $global_path\n"
	else
		printf "❌ global none\n"
	end
end

function ici_yarn
	ici_print_header "Yarn - https://yarnpkg.com"
	printf "📚 https://yarnpkg.com/getting-started\n"

	if type -q yarn
		set --local latest_version (curl --silent 'https://archlinux.org/packages/community/any/yarn/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $latest_version - https://archlinux.org/packages/community/any/yarn/\n"

		# x.y.z
		set --local global_version (yarn --version)
		set --local global_path (command -v yarn)
		printf "✅ global $(ici_print_version $latest_version $global_version) - $global_path\n"
	else
		printf "❌ global none\n"
	end

	# https://yarnpkg.com/configuration/yarnrc
	ici_print_config 'yarnrc'
	ici_print_config 'yarn.lock'
end

function ici_docker
	ici_print_header "Docker - https://www.docker.com"
	printf "📚 https://docs.docker.com\n"

	if type -q docker
		set --local latest_version (curl --silent 'https://archlinux.org/packages/community/x86_64/docker/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $latest_version - https://archlinux.org/packages/community/x86_64/docker/\n"

		# Docker version 20.10.16, build aa7e414fdc
		set --local global_version (docker --version | string sub --start 16 --end 23)
		set --local global_path (command -v docker)
		printf "✅ global $(ici_print_version $latest_version $global_version) - $global_path\n"
	end

	if test $ici_git_repo = true
		ici_print_config 'Dockerfile'
		ici_print_config 'docker-compose'
		ici_print_ignore 'dockerignore'
	end
end

function ici_terraform
	ici_print_header "Terraform - https://www.terraform.io"
	printf "📚 https://www.terraform.io/docs\n"

	if type -q terraform
		set --local latest_version (curl --silent 'https://archlinux.org/packages/community/x86_64/terraform/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $latest_version - https://archlinux.org/packages/community/x86_64/terraform/\n"

		# Terraform v1.1.9
		set --local global_version (terraform --version | head -n 1 | string sub --start 12)
		set --local global_path (command -v terraform)
		printf "✅ global $(ici_print_version $latest_version $global_version) - $global_path\n"
	end

	if test $ici_git_repo = true
		ici_print_config '\.tf'
		ici_print_config 'tfvars'
		ici_print_config '.terraform.lock.hcl'
	end
end

function ici_package
	ici_print_header "Package"

	if test -f package.json
		set --local package_name (jq --raw-output .name package.json)
		set --local package_version (jq --raw-output .version package.json)
		set --local package_workspaces (jq --raw-output .workspaces package.json)

		printf "📦 $package_name@$package_version\n"

		if test "$package_workspaces" != null
			printf "✅ workspaces\n"

			# displaying only the package.json .workspaces keys is not enough
			if type -q yarn
				yarn workspaces list --json | jq --raw-output '"⛺ " + .location + " " + .name'
			end
		else
			printf "❌ workspaces\n"
		end

	# --prune is needed to avoid going to deep
	for node_modules in $(fd --strip-cwd-prefix --color never --hidden --no-ignore --exclude .git --prune --type directory 'node_modules')
		printf "📁 $(du --human-readable --summarize $node_modules)\n"
	end
	end
end

function ici_typescript
	ici_print_header "TypeScript - https://www.typescriptlang.org"
	printf "📚 https://www.typescriptlang.org/docs/\n"

	if type -q tsc
		set --local latest_arch_version (curl --silent 'https://archlinux.org/packages/community/any/typescript/json/' | jq --raw-output .pkgver)
		printf "🌍 latest $latest_arch_version - https://archlinux.org/packages/community/any/typescript/\n"

		set --local latest_version (curl --silent 'https://registry.npmjs.com/typescript' | jq --raw-output '.["dist-tags"].latest')
		printf "🌍 latest $latest_version - https://registry.npmjs.com/typescript\n"

		# Version x.y.z
		set --local global_version (tsc --version | string sub --start=9)
		set --local global_path (command -v tsc)
		printf "✅ global $(ici_print_version $latest_version $global_version) - $global_path\n"
	else
		printf "❌ global none\n"
	end

	if test $ici_git_repo = true
		set --local reported_version (npx tsc --version | string sub --start=9)
		printf "✅ local $reported_version\n"

		ici_print_dev_dependencies 'typescript' '@typescript-eslint/parser'

		for config in $(fd --strip-cwd-prefix --hidden --exclude .git --type file 'tsconfig');
			echo "⚙️ $config $(jq .extends $config 2> /dev/null || echo 'INVALID JSON (extra commas…)')"
		end
	end
end

function ici_prettier
	ici_print_header "Prettier - https://prettier.io"
	printf "📚 https://prettier.io/docs/en/index.html\n"

	set --local prettier_latest_version (curl --silent 'https://registry.npmjs.com/prettier' | jq --raw-output '.["dist-tags"].latest')
	printf "🌍 latest $prettier_latest_version - https://registry.npmjs.com/prettier\n"

	if type -q prettier
		# x.y.z
		set --local prettier_global_version (prettier --version)
		set --local global_path (command -v prettier)
		printf "✅ global $(ici_print_version $latest_version $global_version) - $global_path\n"
	else
		printf "❌ global none\n"
	end

	if test $ici_git_repo = true
		set --local reported_version (npx prettier --version)
		printf "✅ local $reported_version\n"

		if test -f package.json
			ici_print_dev_dependencies 'prettier' 'eslint-config-prettier' 'eslint-plugin-prettier'

			set --local package_config (jq --raw-output .prettier package.json)
			if test "$package_config" != null
				printf "⚙️ package.json\n"
			end
		end

		# https://prettier.io/docs/en/configuration.html
		ici_print_config 'prettierrc'
		ici_print_ignore 'prettierignore'
	end
end

function ici_eslint
	set_color --bold
	printf "\nESLint - https://eslint.org\n"
	set_color normal
	printf "📚 https://eslint.org/docs/user-guide/\n"

	set --local latest_version (curl --silent 'https://registry.npmjs.com/eslint' | jq --raw-output '.["dist-tags"].latest')
	printf "🌍 latest $latest_version - https://registry.npmjs.com/eslint\n"

	if type -q eslint
		# vx.y.z
		set --local global_version (eslint --version | string sub --start=2)
		set --local global_path (command -v eslint)
		printf "✅ global $(ici_print_version $latest_version $global_version) - $global_path\n"
	else
		printf "❌ global none\n"
	end

	if test $ici_git_repo = true
		set --local reported_version (npx eslint --version | string sub --start=2)
		printf "✅ local $reported_version\n"

		ici_print_dev_dependencies 'eslint' 'eslint-config-prettier' 'eslint-config-google' 'eslint-plugin-import' '@typescript-eslint/parser'

		# https://eslint.org/docs/user-guide/configuring/
		for config in $(fd --strip-cwd-prefix --hidden --exclude .git --type file 'eslintrc');
			echo "⚙️ $config $(jq .extends $config 2> /dev/null || echo 'INVALID JSON (extra commas…)')"
		end

		ici_print_ignore 'eslintignore'
	end
end

function ici_jest
	set_color --bold
	printf "\nJest - https://jestjs.io\n"
	set_color normal
	printf "📚 https://jestjs.io/docs/getting-started\n"

	set --local latest_version (curl --silent 'https://registry.npmjs.com/jest' | jq --raw-output '.["dist-tags"].latest')
	printf "🌍 latest $latest_version - https://registry.npmjs.com/jest\n"

	if type -q jest
		# x.y.z
		set --local global_version (jest --version)
		set --local global_path (command -v jest)
		printf "✅ global $(ici_print_version $latest_version $global_version) - $global_path\n"
	else
		printf "❌ global none\n"
	end

	if test $ici_git_repo = true
		set --local reported_version (npx jest --version)
		printf "✅ local $reported_version\n"

		ici_print_dev_dependencies 'jest' '@types/jest' 'ts-jest' 'jest-marbles' '@angular-builders/jest' '@nrwl/jest'

		# https://jestjs.io/docs/configuration
		ici_print_config 'jest\.config'
	end
end

