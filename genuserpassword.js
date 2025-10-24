//#!/usr/bin/env node

const crypto = require("crypto");

// generate 9-character random salt: [a-z0-9]
function generateSalt() {
	const chars = "abcdefghijklmnopqrstuvwxyz0123456789";
	let salt = "";
	for (let i = 0; i < 9; i++) {
		salt += chars[Math.floor(Math.random() * chars.length)];
	}
	return salt;
}

// hash password + salt with SHA-256
function encryptPassword(password, salt) {
	return crypto.createHash("sha256").update(password + salt).digest("hex");
}

// read password from CLI arg
const password = process.argv[2];
if (!password) {
	console.error("Usage: node " + process.argv[1] + " <password>");
	process.exit(1);
}

const salt = generateSalt();
const hash = encryptPassword(password, salt);

console.log("Salt:", salt);
console.log("Hash:", hash);
