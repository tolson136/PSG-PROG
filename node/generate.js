'use strict';

const Excel = require('exceljs');
const fs = require('fs');
const path = require('path');
if (process.argv.length !== 4) throw new Error(`Incorrect arguments: expected 4, found ${process.argv.length}`); 
//const DATA_DIR = path.join(__dirname, 'data');
const DATA_DIR = process.argv[2];
const OUTPUT_FILE = process.argv[3];

const files = fs.readdirSync(DATA_DIR).filter((d) => /\.csv$/.test(d));

console.log(process.argv[2]);

(async function () {
  const workbook = new Excel.Workbook();

  for (let idx = 0; idx < files.length; idx ++) {
    const fileName = files[idx];
    const filePath = path.join(DATA_DIR, fileName);
    const worksheet = await workbook.csv.readFile(filePath);
    worksheet.name = `Route${idx + 1}`;

    const headerRow = worksheet.getRow(1);
    const titleRow = worksheet.getRow(2);
    [headerRow, titleRow].forEach((r) => {
      r.eachCell((cell) => {
        cell.style = {
          font: {
            bold: true
          }
        };
      });
    });

   // worksheet.columns.forEach((col) => {
   //   col.style = {
   //     width: 500
   //   };
   // });
    worksheet.columns = [ 
     {key: 'Location', width: 24},
     {key: 'Address', width: 22},
     {key: 'City', width: 7},
     {key: 'Ticket', width: 9.5},
     {key: 'Freq', width: 6},
     {key: 'Sat', width: 10},
     {key: 'Mon', width: 10},
     {key: 'Tue', width: 10},
     {key: 'Wed', width: 10},
     {key: 'Thu', width: 10},
     {key: 'Fri', width: 10},
     {key: 'Tech', width: 5},
     {key: 'Vol', width: 9},
     {key: 'Cod', width: 6},
     {key: 'Notes', width: 21}
  ]
  //worksheet.getCell('M1').numFmt='00.00';
    const sumColumns = [];
    titleRow.eachCell((cell, number) => {
      if (/vol|cod/i.test(cell.text)) {
        sumColumns.push(number);
      }
    });
    const totalRow = worksheet.addRow();
    sumColumns.forEach((number) => {
      const cell = totalRow.getCell(number);
      const column = worksheet.columns.find((c) => c.number === number);
      cell.value = {
        formula: `=SUM(${column.letter}3:${column.letter}${worksheet.rowCount - 1})`  
		//formula: `=SUM(M3:M${worksheet.rowCount - 1})`
      };
    column.eachCell((cell) => {
      cell.numFmt='_(* #,##0.00_);_(* (#,##0.00);_(* "-"??_);_(@_)';
    })
    });
  }

  await workbook.xlsx.writeFile(OUTPUT_FILE);
})();

