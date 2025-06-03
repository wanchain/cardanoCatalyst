# Use the latest stable Node.js runtime as a parent image
FROM node:20-alpine

# Set the working directory in the container
WORKDIR /usr/src/app

# Copy package.json and pnpm-lock.yaml to the working directory
COPY package.json pnpm-lock.yaml ./

# Install pnpm globally
RUN npm install -g pnpm

# Install dependencies
RUN pnpm install

# Copy the rest of the application code to the working directory
COPY . .

# Build the TypeScript code
RUN pnpm run build

# Expose the port the app runs on (if applicable)
EXPOSE 3000

# Define the command to run the application
CMD ["pnpm", "start"]
