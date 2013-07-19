#!/usr/bin/env python
# -*- coding: utf-8 -*_
#

from gi.repository import Gio, GLib, Gtk, EvinceView, EvinceDocument
import sys, subprocess, os

class PdfPreviewer(Gtk.Window):

	def __init__(self):
		Gtk.Window.__init__(self)

		# Path to the PDF file that is shown in the preview. 
		# This corresponds to the master .tex filed with ".tex"
		# replaced by ".pdf"
		self._filename = None

		# List of tex files loaded by this preview.
		self._tex_files = []

		self.set_title("Tex companion")
		self.connect("destroy", self.destroy)

		self.build_ui()
		self.set_size_request(480, 480)

	def build_ui(self):
		"""Init the UI of the TexCompanion"""

		# Construct a Box that will hold the status bar
		# and that will display the errors when needed
		box = Gtk.Box(orientation = Gtk.Orientation.VERTICAL,
			      spacing = 6)
				
		EvinceDocument.init()
		self._view = EvinceView.View()

		scrolled_window = Gtk.ScrolledWindow()
		scrolled_window.add(self._view)

		box.pack_start(scrolled_window, True, True, 0)

		# Status bar creation
		self._status_bar = Gtk.Label("")
		self._status_bar.set_justify(Gtk.Justification.LEFT)
		self._status_bar.set_alignment(0.0, 0.5)
		self._status_bar.set_padding(6, 0)
		box.pack_start(self._status_bar, False, True, 0)
		
		self.add(box)

	def show_message(self, message):
		self._status_bar.set_markup(message)

		while Gtk.events_pending():
			Gtk.main_iteration()

	def destroy(self, win = None):
		Gtk.main_quit()

	def add_tex_file(self, tex_file):
		self._tex_files.append(tex_file)

		# Add some hooks to update this window when
		# the file is compiled
		tex_file._on_compilation_start.append (lambda : self.show_message("Reloading file..."))
		tex_file._on_compilation_finished.append(lambda error_message : 
							 self.reload_file(error_message))

		self.load_file(tex_file.get_master().get_filename().replace(".tex", ".pdf"))

	def load_file(self, path):
		self._filename = path
		doc = EvinceDocument.Document.factory_get_document ("file://" + 
								    os.path.abspath(path))
		self._model = EvinceView.DocumentModel.new_with_document(doc)
		self._model.set_sizing_mode(EvinceView.SizingMode.BEST_FIT)
		self._view.set_model(self._model)

		self.show_message("Document loaded")

	def reload_file(self, error_message):
		if error_message is not None:
			self.show_message("<b>An error occurred during the compilation</b>:\n" + 
					  error_message)
		elif self._filename is not None:
			current_page = self._model.get_page()
			self.load_file(self._filename)
			self._model.set_page(current_page)

class TexFile():
	
	def __init__(self, filename, main_file = None):
		self._filename = filename
		self._file = Gio.File.new_for_path(self._filename)

		self.reload_hash()
		self._main_file = main_file

		self._monitor = self._file.monitor_file(Gio.FileMonitorFlags.NONE, None)
		self._monitor.connect("changed", self.on_file_changed)

		self._compiling = False

		# List of hooks functions that should be called before
		# and after a compilation. The latter functions will be
		# passed a string describing the error in the compilation, 
		# of True if it didn't fail. 
		self._on_compilation_start = []
		self._on_compilation_finished = []

	def reload_hash(self):
		with open(self._filename) as handle:
			self._hash = hash(handle.read())

	def on_file_changed(self, file, other_file, event_type, user_param):
		old_hash = self._hash
		self.reload_hash()
	
		if old_hash != self._hash:
			self._compiling = True
			self.compile()
			self._compiling = False

	def is_master(self):
		return self._main_file is None

	def get_filename(self):
		return self._filename

	def get_master(self):
		return self if self._main_file is None else self._main_file
		
	def compile(self):

		for hook in self._on_compilation_start:
			hook()

		error_message = ""
		if self._main_file is None:
			# We are the main file, compile!
			p = subprocess.Popen([ "pdflatex", "-interaction", "nonstopmode", 
					       self._filename ], 
					     stdout = subprocess.PIPE)
			output = p.communicate()[0]

			if p.wait() != 0:
				lines = output.split("\n")
				for index in [ lines.index(l) for l in lines 
					       if len(l) > 0 and l[0] == '!' ]:
					error_message += "\n".join(
						lines[index:min(index+3, len(lines))])
		else:
			self._main_file.compile()

		for hook in self._on_compilation_finished:
			hook(error_message if error_message != "" else None)

if __name__ == "__main__":

	preview = PdfPreviewer()
	
	files = sys.argv[1:]
	if len(sys.argv) > 1:
		main_file = TexFile(files[0])
		preview.add_tex_file(main_file)

	for tex_file in files[1:]:
		preview.add_tex_file(TexFile(tex_file, main_file))

	preview.show_all()
	Gtk.main()
		
