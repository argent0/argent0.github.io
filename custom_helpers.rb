module CustomHelpers
	@@base_title = "Aner's Site"
	@@github_url = "https://github.com/argent0"
	def header()
		@@base_title
	end
	def full_title(page_title=nil)
		if page_title.nil?
			@@base_title
		else
			"#{page_title} - #{base_title}"
		end
	end
	def sub_pages(dir)
		sitemap.resources.select do |resource|
			resource.path.start_with?(dir)
		end
	end
	def if_else_path_matches(pattern,true_case,false_case)
		if (current_page.path == pattern)
			true_case
		else
			false_case
		end
	end
	def github_link
		link_to 'Github', @@github_url
	end
	def copyright_str
		"Copyright #{Time.now.year} Aner Lucero"
	end
end
