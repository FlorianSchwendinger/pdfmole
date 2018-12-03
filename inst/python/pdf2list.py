# -*- coding: utf-8 -*-
"""
Created on Wed Feb 18 19:55:12 2015
@Author: Florian Schwendinger
@Email: FlorianSchwendinger@gmx.at
@License: GPL-3
"""

from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter
from pdfminer.converter import PDFLayoutAnalyzer
from pdfminer.layout import (LAParams, LTPage, LTLine, LTRect, LTCurve, LTFigure, 
                             LTTextLine, LTTextBox, LTTextBoxVertical, LTChar, 
                             LTText, LTImage)
from pdfminer.pdfpage import PDFPage
from pdfminer.utils import bbox2str

##  PDFConverter
##
class PDFConverter2(PDFLayoutAnalyzer):

    def __init__(self, rsrcmgr, codec='utf-8', pageno=1, laparams=None):
        PDFLayoutAnalyzer.__init__(self, rsrcmgr, pageno=pageno, laparams=laparams)
        self.codec = codec
        return


##  TextConverter
##
class Text2List(PDFConverter2):

    def __init__(self, rsrcmgr, outfp, codec='utf-8', pageno=1, laparams=None,
                 imagewriter=None):
        PDFConverter2.__init__(self, rsrcmgr, codec=codec, pageno=pageno, laparams=laparams)
        self.doc = list()
        self.imagewriter = imagewriter
        return

    def receive_layout(self, ltpage):
        def render(item):
            if isinstance(item, LTContainer):
                for child in item:
                    render(child)
            elif isinstance(item, LTText):
                self.page.append(item.get_text())
            if isinstance(item, LTTextBox):
                pass
            elif isinstance(item, LTImage):
                if self.imagewriter is not None:
                    self.imagewriter.export_image(item)
        self.page = list()
        render(ltpage)
        self.doc.append(page)
        return

    # Some dummy functions to save memory/CPU when all that is wanted
    # is text.  This stops all the image and drawing ouput from being
    # recorded and taking up RAM.
    def render_image(self, name, stream):
        if self.imagewriter is None:
            return
        PDFConverter2.render_image(self, name, stream)
        return

    def paint_path(self, gstate, stroke, fill, evenodd, path):
        return


class XML2List(PDFConverter2):
    """
        XML2List
    """    
    def __init__(self, rsrcmgr, codec='utf-8', pageno=1,
                 laparams=None, imagewriter=None, stripcontrol=False):
        PDFConverter2.__init__(self, rsrcmgr, codec=codec, pageno=pageno, laparams=laparams)
        self.page = dict()
        self.doc = list()
        self.imagewriter = imagewriter
        self.stripcontrol = stripcontrol
        return
        
    def receive_layout(self, ltpage):
        def render(item):
            if isinstance(item, LTPage):
                self.page = dict()
                self.page['metainfo'] = {'id': item.pageid, 'bbox': item.bbox, 'rotate': item.rotate}
                self.page['text'] = list()
                self.page['line'] = list()
                self.page['rect'] = list()
                self.page['curve'] = list()
                self.page['figure'] = list()
                self.page['textline'] = list()
                self.page['textbox'] = list()
                self.page['image'] = list()
                for child in item:
                    render(child)
                self.doc.append(self.page)
            elif isinstance(item, LTLine):
                self.page['line'].append({'linewidth': item.linewidth, 
                                          'bbox': bbox2str(item.bbox)})
            elif isinstance(item, LTRect):
                self.page['rect'].append({'linewidth': item.linewidth, 
                                          'bbox': bbox2str(item.bbox)})
            elif isinstance(item, LTCurve):
                self.page['curve'].append({'linewidth': item.linewidth, 
                                           'bbox': bbox2str(item.bbox),
                                           'pts': item.get_pts()})
            elif isinstance(item, LTFigure):
                self.page['figure'].append({'name': item.name, 
                                            'bbox': bbox2str(item.bbox)})
                for child in item:
                    render(child)
            elif isinstance(item, LTTextLine):
                self.page['textline'].append({'bbox': bbox2str(item.bbox)})
                for child in item:
                    render(child)
            elif isinstance(item, LTTextBox):
                wmode = ''
                if isinstance(item, LTTextBoxVertical):
                    wmode = 'vertical'
                self.page['textbox'].append({'id': item.index,
                                             'bbox': bbox2str(item.bbox),
                                             'wmode': wmode})
                for child in item:
                    render(child)
            elif isinstance(item, LTChar):
                self.page['text'].append({'font': item.fontname, 
                                          'bbox': item.bbox, 
                                          'size': item.size,
                                          'text': item.get_text()
                                          })
            elif isinstance(item, LTText):
                self.page['text'].append({'text': item.get_text()})
            elif isinstance(item, LTImage):
                if self.imagewriter is not None:
                    name = self.imagewriter.export_image(item)
                    self.page['image'].append({'src': enc(name),
                                               'width': item.width,
                                               'height': item.height})
                else:
                    self.page['image'].append({'width': item.width,
                                               'height': item.height})
            else:
                assert 0, item
            return
        render(ltpage)
        return

def pdf2list(path, pageno=1, codec='utf-8', stripcontrol=False, check_extractable=True, password = "", caching=True, pagenos=[], maxpages = 0):
    rsrcmgr = PDFResourceManager()
    laparams = LAParams()
    device = XML2List(rsrcmgr, codec=codec, pageno=pageno, laparams=laparams, stripcontrol=stripcontrol)
    fp = file(path, 'rb')
    interpreter = PDFPageInterpreter(rsrcmgr, device)
    
    if ( (pagenos is None) or (len(pagenos) == 0) ):
        pagenos = [i[0] for i in enumerate(PDFPage.get_pages(fp))]

    pagenos = set(pagenos)
    
    for page in PDFPage.get_pages(fp, pagenos, maxpages=maxpages, password=password, caching=caching, check_extractable=True):
        interpreter.process_page(page)
    
    return( device.doc )


def pdf2listText(path, pageno=1, codec='utf-8', stripcontrol=False, 
    check_extractable=True, password = "", caching=True, pagenos=[], maxpages = 0):
    rsrcmgr = PDFResourceManager()
    laparams = LAParams()
    device = Text2List(rsrcmgr, codec=codec, pageno=pageno, laparams=laparams, 
                       imagewriter=None)
    fp = file(path, 'rb')
    interpreter = PDFPageInterpreter(rsrcmgr, device)
    pagenos = set(pagenos)
    
    for page in PDFPage.get_pages(fp, pagenos, maxpages=maxpages, password=password, caching=caching, check_extractable=True):
        interpreter.process_page(page)
    
    return( device.doc )

#if __name__ == "__main__":
#    x = pdf2list("classification.pdf")
